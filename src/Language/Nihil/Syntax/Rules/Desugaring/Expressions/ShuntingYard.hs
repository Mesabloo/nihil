-- The Great Nihil Compiler
-- Copyright (c) 2019 Mesabloo

-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.

-- You should have received a copy of the GNU General Public License
-- along with this program. If not, see <http://www.gnu.org/licenses/>.

module Language.Nihil.Syntax.Rules.Desugaring.Expressions.ShuntingYard where

import Language.Nihil.Syntax.Internal.Parsing.Located
import qualified Language.Nihil.Syntax.Internal.Parsing.AST as P
import qualified Language.Nihil.Syntax.Internal.Desugaring.CoreAST as D
import Language.Nihil.Syntax.Desugarer (Desugarer, fixities)
import Language.Nihil.Syntax.Rules.Desugaring.Pattern
import Language.Nihil.Syntax.Internal.Desugaring.Errors.UnexpectedOperator
import Language.Nihil.Syntax.Rules.Desugaring.Types.Type
import {-# SOURCE #-} Language.Nihil.Syntax.Rules.Desugaring.Toplevel.Statement
import qualified Data.Map as Map
import Control.Monad.Except (throwError)
import Control.Lens (use, (^.))
import Control.Monad (forM)
import Data.Composition ((.:))
import Data.Maybe (catMaybes)

-- | Run the Shunting Yard Algorithm on an expression.
syExpr :: String                -- ^ File name
          -> P.Expr             -- ^ Input expression
          -> [Located D.Expr] -- ^ Output stack
          -> [String]           -- ^ Operator stack
          -> Desugarer (Located D.Expr)
syExpr fileName [] out ops =
    if null out
    then pure $ D.EId "()" :@ Nothing
    else addOperators ops out
  where addOperators :: [String] -> [Located D.Expr] -> Desugarer (Located D.Expr)
        -- ^ Adds the operators on the output stack when there is nothing left to desugar.
        addOperators [] out = pure $ head out
        addOperators (o:os) out =
            if length out < 2
            then throwError $ makeUnexpectedOperatorError o
            else let (e1:e2:es) = out
                 in addOperators os ((D.EApp (D.EApp (D.EId o :@ Nothing) e2 :@ Nothing) e1 :@ Nothing) : es)
syExpr fileName ((P.AOperator o :@ _):xs) out ops = do
    (out', ops') <- handleOperator o out ops
    syExpr fileName xs out' ops'
  where
    handleOperator :: String -> [Located D.Expr] -> [String] -> Desugarer ([Located D.Expr], [String])
    -- ^ Adds the operator on top of the operator stack, popping other operators if needed
    handleOperator o out ops = do
        ops' <- use fixities

        if null ops
        then pure (out, [o])
        else
            let (o1:os) = ops
                P.Infix assoc1 prec1 _ = ops' Map.! o1
                P.Infix _ prec2 _ = ops' Map.! o
            in  if (prec1 > prec2 || (prec1 == prec2 && assoc1 == P.L)) && o1 /= "("
                then
                    if length out < 2
                    then throwError $ makeUnexpectedOperatorError o1
                    else
                        let (e1:e2:es') = out
                        in handleOperator o ((D.EApp (D.EApp (D.EId o1 :@ Nothing) e2 :@ Nothing) e1 :@ Nothing) : es') os
                else pure (out, o : ops)
syExpr fileName ((x :@ p):xs) out ops = do
    ops' <- use fixities

    e <- case x of
        P.AId id' -> pure (D.EId id' :@ p)
        P.AHole -> pure (D.EHole :@ p)
        P.ALit (P.LStr s) -> pure $ foldr (\t acc -> D.EApp (D.EApp (D.EId ":" :@ Nothing) (D.ELit (D.LChr t) :@ Nothing) :@ Nothing) (acc :@ Nothing)) (D.EId "[]") s :@ p
        P.ALit (P.LChr c) -> pure $ D.ELit (D.LChr c) :@ p
        P.ALit (P.LInt i) -> pure $ D.ELit (D.LInt i) :@ p
        P.ALit (P.LDec d) -> pure $ D.ELit (D.LDec d) :@ p
        P.AApp a1 a2 -> do
            e1 <- syExpr fileName [a1] [] []
            e2 <- syExpr fileName [a2] [] []

            pure (D.EApp e1 e2 :@ p)
        P.AParens (a :@ _) -> syExpr fileName a [] []
        P.ALambda ss (e :@ p1) -> do
            e' <- syExpr fileName e [] []
            args <- mapM (desugarPattern fileName . (: [])) ss

            pure $ foldr (flip (:@) Nothing .: D.ELam) e' args
        P.ATuple es -> do
            es' <- forM es $ \(e :@ _) -> syExpr fileName e [] []

            pure $ D.ETuple es' :@ p
        P.AList es -> do
            es' <- forM es $ \(e :@ _) -> syExpr fileName e [] []

            pure $ ((foldr (\t acc -> D.EApp (D.EApp (D.EId ":" :@ Nothing) t :@ Nothing) acc :@ Nothing) (D.EId "[]" :@ Nothing) es') :@ p) ^. located
        P.AMatch (e :@ p1) cases -> do
            toMatch <- syExpr fileName e [] []
            branches <- mapM (uncurry handleBranch) cases

            pure $ D.EMatch toMatch branches :@ p
          where
            handleBranch :: [Located P.Pattern] -> Located P.Expr -> Desugarer (Located D.Pattern, Located D.Expr)
            handleBranch [] _ = undefined
            handleBranch pats (branch :@ p1) = do
                p' <- desugarPattern fileName pats
                e1 <- syExpr fileName branch [] []

                pure (p', e1)
        P.AAnn (e :@ p1) t -> do
            e' <- syExpr fileName e [] []
            t' <- desugarType "" t

            pure $ D.EAnn e' t' :@ p
        P.ALet ss (e2 :@ p2) -> do
            ss' <- catMaybes <$> mapM (desugarStatement fileName) ss
            e2' <- syExpr fileName e2 [] []
            pure $ D.ELet ss' e2' :@ p
        P.AWhere e ss -> syExpr fileName ((P.ALet ss e :@ p):xs) out ops
        P.AOperator _ -> undefined -- ! should never happen

    syExpr fileName xs (e:out) ops