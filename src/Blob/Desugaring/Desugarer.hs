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
import Blob.Desugaring.ShuntingYard

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
    ops' <- gets D.fixities

    let (e :- p) = expr

    let expr' = runExcept $ runStateT (syExpr e) (initSYState ops')

    case expr' of
        Left err -> throwError err
        Right (_, state') -> pure (head $ D.output state')

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

-- runners

runDesugarer :: String -> Annotated P.Program -> D.Sugar (Annotated D.Program)
runDesugarer fileName program = do
    accumulateOnProgram program

    desugarProgram fileName program

runSugar :: D.Sugar a -> D.SugarState -> Either Doc (a, D.SugarState)
runSugar = runExcept .: runStateT
