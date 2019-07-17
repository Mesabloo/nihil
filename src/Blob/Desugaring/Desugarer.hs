{-# LANGUAGE LambdaCase #-}

module Blob.Desugaring.Desugarer where

import qualified Blob.Desugaring.Types as D
import qualified Blob.Parsing.Types as P
import qualified Data.Map as Map
import Control.Applicative
import Control.Monad.State
import Control.Monad.Except
import Text.PrettyPrint.Leijen (text, dot, linebreak, Doc)
import qualified Data.Set as Set
import qualified Data.MultiMap as MMap
import Blob.Desugaring.Defaults
import Text.Megaparsec
import Control.Monad.Combinators.Expr
import Blob.Parsing.Annotation
import Data.List (sortBy)
import Data.Foldable (foldrM, foldlM)
import Data.Maybe (fromJust)
import Data.Composition ((.:))
import Data.Functor ((<&>))
import Blob.Desugaring.Errors

desugarProgram :: String -> Annotated P.Program -> D.Sugar (Annotated D.Program)
desugarProgram fileName (s :- p)= do
    x <- mapM (desugarStatement fileName) s
    let l = filter (/= (D.Empty :- Nothing)) x
    pure (D.Program l :- p)

desugarStatement :: String -> Annotated P.Statement -> D.Sugar (Annotated D.Statement)
desugarStatement fileName (P.Declaration name pType :- p) = do
    t <- desugarType fileName pType
    pure (D.Declaration name t :- p)
desugarStatement fileName (P.Definition name args pExpr :- p) = do
    e <- desugarExpression fileName pExpr
    let val = foldr (\a acc -> D.ELam a acc :- Nothing) e args
    pure (D.Definition name val :- p)
desugarStatement fileName (P.TypeDeclaration name tvs cType :- p) = do
    ct <- desugarCustomType fileName (name, tvs, cType)
    pure (D.TypeDeclaration name tvs ct :- p)
desugarStatement fileName (_ :- p) = pure (D.Empty :- p)

desugarCustomType :: String -> (String, [String], Annotated P.CustomType) -> D.Sugar (Annotated D.CustomType)
desugarCustomType fileName (name, tvs, ct :- p) = case ct of
    P.TAlias t -> do
        t' <- desugarType fileName t
        pure $ D.TAlias t' :- p
    P.TSum s -> do
        state' <- get

        let initialType = foldl (flip (:-) Nothing .: D.TApp) (D.TId name :- Nothing) (map (flip (:-) Nothing . D.TVar) tvs)
            folded = foldr (flip (:-) Nothing .: D.TFun) initialType
            m = Map.map (\cs -> do
                cs' <- traverse (desugarType fileName) cs
                pure (D.Scheme tvs $ folded cs')) s
        m' <- sequence m

        pure (D.TSum m' :- p)

desugarType :: String -> Annotated P.Type -> D.Sugar (Annotated D.Type)
desugarType fileName (P.TId name :- p) = pure (D.TId name :- p)
desugarType fileName (P.TApp [] :- _) = undefined
desugarType fileName (P.TApp (t:ts) :- p) = do
    t1 <- desugarType fileName t

    foldlM (\acc t' -> do
        t2 <- desugarType fileName t'
        let Just (beg, _) = getSpan acc
            Just (_, end) = getSpan t2

        pure (D.TApp acc t2 :- Just (beg, end)) ) t1 ts
desugarType fileName (P.TVar name :- p) = pure (D.TVar name :- p)
desugarType fileName (P.TFun t1 t2 :- p) = do
    t1' <- desugarType fileName t1
    t2' <- desugarType fileName t2
    pure (D.TFun t1' t2' :- p)
desugarType fileName (P.TArrow e t1 t2 :- p) = do
    e' <- desugarExpression fileName e
    t1' <- desugarType fileName t1
    t2' <- desugarType fileName t2

    pure (D.TArrow e' t1' t2' :- p)
desugarType fileName (P.TTuple ts :- p) = do
    ts' <- mapM (desugarType fileName) ts

    pure (D.TTuple ts' :- p)
desugarType fileName (P.TList ts :- p) =
    flip (:-) p . getAnnotated <$> foldlM (\acc t -> do
        t' <- desugarType fileName t

        pure (D.TApp acc t' :- Nothing) ) (D.TId "[]" :- Nothing) ts

desugarExpression :: String -> Annotated P.Expr -> D.Sugar (Annotated D.Expr)
desugarExpression fileName expr = do
    let (e :- p) = expr

    syExpr e [] []


-- operators accumulator

accumulateOnProgram :: Annotated P.Program -> D.Sugar ()
accumulateOnProgram (p :- _) = do
    let customOps = filter check p
        other = filter (not . check) p

    mapM_ accumulateOnStatement customOps
    mapM_ accumulateOnStatement other
  where check (P.OpFixity _ _ :- _) = True
        check _ = False

accumulateOnStatement :: Annotated P.Statement -> D.Sugar ()
accumulateOnStatement (P.OpFixity _ (f@(P.Infix _ _ name) :- _) :- _) =
    modify $ \st -> st { D.fixities = Map.insert name f (D.fixities st) }
accumulateOnStatement (P.Definition _ _ expr :- _) = accumulateOnExpression expr
accumulateOnStatement _ = pure ()

accumulateOnExpression :: Annotated P.Expr -> D.Sugar ()
accumulateOnExpression = mapM_ accumulateOnAtom . getAnnotated

accumulateOnAtom :: Annotated P.Atom -> D.Sugar ()
accumulateOnAtom (P.AOperator name :- _) = modify $ \st -> st { D.fixities = Map.insertWith (flip const) name (P.Infix P.L 9 name) (D.fixities st) }
accumulateOnAtom (P.AList e :- _) = mapM_ accumulateOnExpression e
accumulateOnAtom (P.ATuple e :- _) = mapM_ accumulateOnExpression e
accumulateOnAtom (P.ALambda _ e :- _) = accumulateOnExpression e
accumulateOnAtom (P.AMatch e pats :- _) = do
    accumulateOnExpression e
    mapM_ (accumulateOnPatterns . fst) pats
    mapM_ (accumulateOnExpression . snd) pats
accumulateOnAtom (P.AParens e :- _) = accumulateOnExpression e
accumulateOnAtom (P.AApp a1 a2 :- _) = do
    accumulateOnAtom a1
    accumulateOnAtom a2
accumulateOnAtom _ = pure ()

accumulateOnPatterns :: [Annotated P.Pattern] -> D.Sugar ()
accumulateOnPatterns = mapM_ accumulateOnPattern

accumulateOnPattern :: Annotated P.Pattern -> D.Sugar ()
accumulateOnPattern (P.POperator name :- _) = modify $ \st -> st { D.fixities = Map.insertWith (flip const) name (P.Infix P.L 9 name) (D.fixities st) }
accumulateOnPattern (P.PCtor _ ps :- _) = mapM_ accumulateOnPatterns ps
accumulateOnPattern (P.PList ps :- _) = mapM_ accumulateOnPatterns ps
accumulateOnPattern (P.PTuple ps :- _) = mapM_ accumulateOnPatterns ps
accumulateOnPattern (P.PParens p :- _) = accumulateOnPatterns p
accumulateOnPattern _ = pure ()

--------------------------------------------------------------------------------------------------------------------
{- runners -}

runDesugarer :: String -> Annotated P.Program -> D.Sugar (Annotated D.Program)
runDesugarer fileName program = do
    accumulateOnProgram program

    desugarProgram fileName program

runSugar :: D.Sugar a -> D.SugarState -> Either Doc (a, D.SugarState)
runSugar = runExcept .: runStateT

--------------------------------------------------------------------------------------------------------------------
{- shunting yard -}

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