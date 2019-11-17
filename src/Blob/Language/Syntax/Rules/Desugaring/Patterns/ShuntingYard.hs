-- Blobc, a compiler for compiling Blob source code
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

module Blob.Language.Syntax.Rules.Desugaring.Patterns.ShuntingYard where

import Blob.Language.Syntax.Internal.Parsing.Located
import qualified Blob.Language.Syntax.Internal.Parsing.AST as P
import qualified Blob.Language.Syntax.Internal.Desugaring.CoreAST as D
import Blob.Language.Syntax.Desugarer (Desugarer, fixities)
import Blob.Language.Syntax.Rules.Desugaring.Types.Type
import Blob.Language.Syntax.Internal.Desugaring.Errors.UnexpectedOperator
import qualified Data.Map as Map
import Control.Lens (use, (^.))
import Control.Monad (forM)
import Control.Monad.Except (throwError)

-- | Runs the Shunting Yard Algorithm on a 'P.Pattern'.
syPat :: String                   -- ^ File name
         -> [Located P.Pattern] -- ^ Input pattern
         -> [Located D.Pattern] -- ^ Output stack
         -> [String]              -- ^ Operator stack
         -> Desugarer (Located D.Pattern)
syPat fileName [] out ops =
    if null out
    then pure $ D.PId "()" :@ Nothing
    else addOperators out ops
  where addOperators :: [Located D.Pattern] -> [String] -> Desugarer (Located D.Pattern)
        -- ^ Adds the operators on the output stack when there is nothing left to desugar.
        addOperators out [] = pure $ head out
        addOperators out (o:os) =
            if length out < 2
            then throwError $ makeUnexpectedOperatorError o
            else let (e1:e2:es) = out
                 in addOperators ((D.PCtor o [e2, e1] :@ Nothing) : es) os
syPat fileName ((P.POperator o :@ p):xs) out ops = do
    (out', ops') <- handleOperator o out ops

    syPat fileName xs out' ops'
  where handleOperator :: String -> [Located D.Pattern] -> [String] -> Desugarer ([Located D.Pattern], [String])
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
                        else do
                            let (e1:e2:es') = out
                            handleOperator o ((D.PCtor o1 [e2, e1] :@ Nothing) : es') os
                    else pure (out, o:os)
syPat fileName ((x :@ p):xs) out ops = do
    pat <- case x of
        P.PHole -> pure D.Wildcard
        P.PId id' -> pure $ D.PId id'
        P.PLit (P.LStr s) -> pure $ foldr (\t acc -> D.PCtor ":" [D.PChr t :@ Nothing, acc :@ Nothing]) (D.PCtor "[]" []) s
        P.PLit (P.LChr c) -> pure $ D.PChr c
        P.PLit (P.LInt i) -> pure $ D.PInt i
        P.PLit (P.LDec d) -> pure $ D.PDec d
        P.PCtor id' pats -> do
            ps <- forM pats $ \p -> syPat fileName p [] []
            pure $ D.PCtor id' ps
        P.PTuple pats -> do
            ps <- forM pats $ \p -> syPat fileName p [] []
            pure $ D.PTuple ps
        P.PParens pat -> (^. located) <$> syPat fileName pat [] []
        P.PList pats -> do
            ps <- forM pats $ \p -> syPat fileName p [] []
            pure $ foldr (\t acc -> D.PCtor ":" [t, acc] :@ Nothing) (D.PCtor "[]" [] :@ Nothing) ps ^. located
        P.PAnn p t -> do
            p' <- syPat fileName p [] []
            t' <- desugarType "" t

            pure $ D.PAnn p' t'
        P.POperator _ -> undefined -- ! Should never happen

    syPat fileName xs ((pat :@ p) : out) ops