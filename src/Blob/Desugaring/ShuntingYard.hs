module Blob.Desugaring.ShuntingYard where

import qualified Blob.Desugaring.Types as D
import qualified Blob.Parsing.Types as P
import Blob.Parsing.Annotation
import qualified Data.Map as Map
import Control.Monad.State
import Data.Composition ((.:))
import Control.Monad.Except
import Blob.Desugaring.Errors
import Blob.Desugaring.Defaults
import Debug.Trace

-- Shunting Yard Algorithm for expressions

syExpr :: P.Expr      -- ^ Input expression
          -> [Annotated D.Expr] -- ^ Output stack
          -> [String] -- ^ Operator stack
          -> D.Sugar (Annotated D.Expr)
syExpr [] out ops =
    if null out
    then pure $ D.EId "()" :- Nothing
    else addOperators ops out
  where addOperators :: [String] -> [Annotated D.Expr] -> D.Sugar (Annotated D.Expr)
        addOperators [] out = pure $ head out
        addOperators (o:os) out = 
            if length out < 2
            then throwError $ makeUnexpectedOperatorError o
            else let (e1:e2:es) = out
                 in addOperators os ((D.EApp (D.EApp (D.EId o :- Nothing) e2 :- Nothing) e1 :- Nothing) : es)
syExpr ((P.AOperator o :- _):xs) out ops = do
    (out', ops') <- handleOperator o out ops
    syExpr xs out' ops'
  where 
    handleOperator :: String -> [Annotated D.Expr] -> [String] -> D.Sugar ([Annotated D.Expr], [String])
    handleOperator o out ops = do
        ops' <- gets D.fixities

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
                        in handleOperator o ((D.EApp (D.EApp (D.EId o1 :- Nothing) e2 :- Nothing) e1 :- Nothing) : es') os
                else pure (out, o : os)
syExpr ((x :- p):xs) out ops = do
    ops' <- gets D.fixities

    e <- case x of
        P.AId id' -> pure (D.EId id' :- p)
        P.AHole -> pure (D.EHole :- p)
        P.ALit (P.LStr s) -> pure $ foldr (\t acc -> D.EApp (D.EApp (D.EId ":" :- Nothing) (D.ELit (D.LChr t) :- Nothing) :- Nothing) (acc :- Nothing)) (D.EId "[]") s :- p
        P.ALit (P.LChr c) -> pure $ D.ELit (D.LChr c) :- p
        P.ALit (P.LInt i) -> pure $ D.ELit (D.LInt i) :- p
        P.ALit (P.LDec d) -> pure $ D.ELit (D.LDec d) :- p
        P.AApp a1 a2 -> do
            e1 <- syExpr [a1] [] []
            e2 <- syExpr [a2] [] []

            pure (D.EApp e1 e2 :- p)
        P.AParens (a :- _) -> syExpr a [] []
        P.ALambda ss (e :- p1) -> do
            e' <- syExpr e [] []

            pure $ foldr (flip (:-) Nothing .: D.ELam) e' ss
        P.ATuple es -> do
            es' <- forM es $ \(e :- _) -> syExpr e [] []

            pure $ D.ETuple es' :- p
        P.AList es -> do
            es' <- forM es $ \(e :- _) -> syExpr e [] []

            pure $ getAnnotated (foldr (\t acc -> D.EApp (D.EApp (D.EId ":" :- Nothing) t :- Nothing) acc :- Nothing) (D.EId "[]" :- Nothing) es') :- p
        P.AMatch (e :- p1) cases -> do
            toMatch <- syExpr e [] []
            branches <- mapM (uncurry handleBranch) cases 

            pure $ D.EMatch toMatch branches :- p
          where
            handleBranch :: [Annotated P.Pattern] -> Annotated P.Expr -> D.Sugar (Annotated D.Pattern, Annotated D.Expr)
            handleBranch [] _ = undefined
            handleBranch pats (branch :- p1) = do
                p' <- syPat pats [] []
                e1 <- syExpr branch [] []

                pure (p', e1)
        P.AOperator _ -> undefined -- ! should never happen

    syExpr xs (e:out) ops

-- shunting yard for patterns

syPat :: [Annotated P.Pattern] -- ^ Input pattern
         -> [Annotated D.Pattern] -- ^ Output stack
         -> [String]
         -> D.Sugar (Annotated D.Pattern)
syPat [] out ops =
    if null out
    then pure $ D.PId "()" :- Nothing
    else addOperators out ops
  where addOperators :: [Annotated D.Pattern] -> [String] -> D.Sugar (Annotated D.Pattern)
        addOperators out [] = pure $ head out
        addOperators out (o:os) =
            if length out < 2
            then throwError $ makeUnexpectedOperatorError o
            else let (e1:e2:es) = out
                 in addOperators ((D.PCtor o [e2, e1] :- Nothing) : es) os
syPat ((P.POperator o :- p):xs) out ops = do
    (out', ops') <- handleOperator o out ops

    syPat xs out' ops'
    where handleOperator :: String -> [Annotated D.Pattern] -> [String] -> D.Sugar ([Annotated D.Pattern], [String])
          handleOperator o out ops = do
            ops' <- gets D.fixities

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
                            handleOperator o ((D.PCtor o1 [e2, e1] :- Nothing) : es') os
                    else pure (out, o:os)
syPat ((x :- p):xs) out ops = do
    pat <- case x of
        P.PHole -> pure D.Wildcard
        P.PId id' -> pure $ D.PId id'
        P.PLit (P.LStr s) -> pure $ foldr (\t acc -> D.PCtor ":" [D.PChr t :- Nothing, acc :- Nothing]) (D.PCtor "[]" []) s
        P.PLit (P.LChr c) -> pure $ D.PChr c
        P.PLit (P.LInt i) -> pure $ D.PInt i
        P.PLit (P.LDec d) -> pure $ D.PDec d
        P.PCtor id' pats -> do
            ps <- forM pats $ \p -> syPat p [] []
            pure $ D.PCtor id' ps
        P.PTuple pats -> do
            ps <- forM pats $ \p -> syPat p [] []
            pure $ D.PTuple ps
        P.PParens pat -> getAnnotated <$> syPat pat [] []
        P.PList pats -> do
            ps <- forM pats $ \p -> syPat p [] []
            pure . getAnnotated $ foldr (\t acc -> D.PCtor ":" [t, acc] :- Nothing) (D.PCtor "[]" [] :- Nothing) ps
        P.POperator _ -> undefined -- ! Should never happen

    syPat xs ((pat :- p) : out) ops