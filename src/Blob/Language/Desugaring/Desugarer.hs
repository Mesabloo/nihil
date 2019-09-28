{-# LANGUAGE LambdaCase #-}

-- | This module holds all the functions used in the desugaring process.
module Blob.Language.Desugaring.Desugarer where

import qualified Blob.Language.Desugaring.Types as D
import qualified Blob.Language.Parsing.Types as P
import qualified Data.Map as Map
import Control.Monad.State
import Control.Monad.Except
import Text.PrettyPrint.Leijen (Doc)
import Blob.Language.Parsing.Annotation
import Data.Foldable (foldlM)
import Data.Composition ((.:))
import Blob.Language.Desugaring.Errors
import Blob.Language.Lexing.Types (SourceSpan(..))
import Data.Maybe

-- | Desugars a whole 'P.Program' into a 'D.Program'.
desugarProgram :: String -> Annotated P.Program -> D.Sugar (Annotated D.Program)
desugarProgram fileName (s :- p)= do
    l <- catMaybes <$> mapM (desugarStatement fileName) s
    pure (D.Program l :- p)

-- | Desugars a 'P.Statement' into a 'D.Statement'.
desugarStatement :: String -> Annotated P.Statement -> D.Sugar (Maybe (Annotated D.Statement))
desugarStatement fileName (P.Declaration name pType :- p) = do
    t <- desugarType fileName pType
    pure $ Just (D.Declaration name t :- p)
desugarStatement fileName (P.Definition name args pExpr :- p) = do
    e <- desugarExpression fileName pExpr
    a <- mapM (\p -> syPat [p] [] []) args
    let val = foldr (\a acc -> D.ELam a acc :- Nothing) e a
    pure $ Just (D.Definition name val :- p)
desugarStatement fileName (P.TypeDeclaration name tvs cType :- p) = do
    ct <- desugarCustomType fileName (name, tvs, cType)
    pure $ Just (D.TypeDeclaration name tvs ct :- p)
desugarStatement _ (_ :- p) = pure Nothing

-- Desugars a 'P.CustomType' into a 'D.CustomType'
desugarCustomType :: String -> (String, [String], Annotated P.CustomType) -> D.Sugar (Annotated D.CustomType)
desugarCustomType fileName (name, tvs, ct :- p) = case ct of
    P.TAlias t -> do
        t' <- desugarType fileName t
        pure $ D.TAlias t' :- p
    P.TSum s -> do
        {- How to desugar a sum type:
            * First transform the base type into a type application
            * Then concat this type with the type of the constructor: `consType <> defType`
        -}
        state' <- get

        let initialType = foldl (flip (:-) Nothing .: D.TApp) (D.TId name :- Nothing) (map (flip (:-) Nothing . D.TVar) tvs)
            folded = foldr (flip (:-) Nothing .: D.TFun) initialType
            m = Map.map (\cs -> do
                cs' <- traverse (desugarType fileName) cs
                pure (D.Scheme tvs $ folded cs')) s
        m' <- sequence m

        pure (D.TSum m' :- p)
    P.TGADT s -> do -- Desugaring a 'P.TGADT' is equivalent to transforming it into a 'D.TSum'
        let m = Map.map (\c -> do
                c' <- desugarType fileName c
                pure (D.Scheme tvs c')) s
        m' <- sequence m

        pure (D.TSum m' :- p)

-- | Desugars a 'P.Type' into a 'D.Type'.
desugarType :: String -> Annotated P.Type -> D.Sugar (Annotated D.Type)
desugarType _ (P.TId name :- p) = pure (D.TId name :- p)
desugarType _ (P.TApp [] :- _) = undefined -- ! never happening
desugarType fileName (P.TApp (t:ts) :- p) = do
    t1 <- desugarType fileName t

    foldlM (\acc t' -> do
        t2 <- desugarType fileName t'
        let Just (SourceSpan beg _) = getSpan acc
            Just (SourceSpan _ end) = getSpan t2

        pure (D.TApp acc t2 :- Just (SourceSpan beg end)) ) t1 ts
desugarType _ (P.TVar name :- p) = pure (D.TVar name :- p)
desugarType fileName (P.TFun t1 t2 :- p) = do
    t1' <- desugarType fileName t1
    t2' <- desugarType fileName t2
    pure (D.TFun t1' t2' :- p)
desugarType fileName (P.TTuple ts :- p) = do
    ts' <- mapM (desugarType fileName) ts

    pure (D.TTuple ts' :- p)
desugarType fileName (P.TList ts :- p) =
    flip (:-) p . getAnnotated <$> foldlM (\acc t -> do
        t' <- desugarType fileName t

        pure (D.TApp acc t' :- Nothing) ) (D.TId "[]" :- Nothing) ts
desugarType fileName (P.TNonLinear t :- p) =
    (:- p) . D.TBang <$> desugarType fileName t

-- | Desugars a 'P.Expr' into a 'D.Expr'.
desugarExpression :: String -> Annotated P.Expr -> D.Sugar (Annotated D.Expr)
desugarExpression _ expr = do
    let (e :- p) = expr

    syExpr e [] []


-- operators accumulator

-- | Accumulates operator fixities in a 'P.Program', beginning with 'P.OpFixity' statements.
accumulateOnProgram :: Annotated P.Program -> D.Sugar ()
accumulateOnProgram (p :- _) = do
    let customOps = filter check p
        other = filter (not . check) p

    mapM_ accumulateOnStatement customOps
    mapM_ accumulateOnStatement other
  where check (P.OpFixity _ _ :- _) = True
        check _ = False

-- | Accumulates operator fixites in a 'P.Statement'.
accumulateOnStatement :: Annotated P.Statement -> D.Sugar ()
accumulateOnStatement (P.OpFixity _ (f@(P.Infix _ _ name) :- _) :- _) =
    modify $ \st -> st { D.fixities = Map.insert name f (D.fixities st) }
accumulateOnStatement (P.Definition _ _ expr :- _) = accumulateOnExpression expr
accumulateOnStatement _ = pure ()

-- | Accumulates operator fixities in a 'P.Expr'.
accumulateOnExpression :: Annotated P.Expr -> D.Sugar ()
accumulateOnExpression = mapM_ accumulateOnAtom . getAnnotated

-- | Accumulates operator fixities in a 'P.Atom'.
--
-- In case of an unregistered operator being encountered, a default fixity of `infixl 9` is registered.
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

-- | Accumulates operator fixities in 'P.Pattern's.
accumulateOnPatterns :: [Annotated P.Pattern] -> D.Sugar ()
accumulateOnPatterns = mapM_ accumulateOnPattern

-- | Accumulates operator fixities in a 'P.Pattern'.
accumulateOnPattern :: Annotated P.Pattern -> D.Sugar ()
accumulateOnPattern (P.POperator name :- _) = modify $ \st -> st { D.fixities = Map.insertWith (flip const) name (P.Infix P.L 9 name) (D.fixities st) }
accumulateOnPattern (P.PCtor _ ps :- _) = mapM_ accumulateOnPatterns ps
accumulateOnPattern (P.PList ps :- _) = mapM_ accumulateOnPatterns ps
accumulateOnPattern (P.PTuple ps :- _) = mapM_ accumulateOnPatterns ps
accumulateOnPattern (P.PParens p :- _) = accumulateOnPatterns p
accumulateOnPattern _ = pure ()

--------------------------------------------------------------------------------------------------------------------
{- runners -}

-- | Runs the entire desugaring process on a given 'P.Program'.
runDesugarer :: String -> Annotated P.Program -> D.Sugar (Annotated D.Program)
runDesugarer fileName program = do
    accumulateOnProgram program

    desugarProgram fileName program

-- | Runs an action in the 'D.Sugar' monad with a given state.
runSugar :: D.Sugar a -> D.SugarState -> Either Doc (a, D.SugarState)
runSugar = runExcept .: runStateT

--------------------------------------------------------------------------------------------------------------------
{- shunting yard -}

-- Shunting Yard Algorithm for expressions

-- | Run the Shunting Yard Algorithm on an expression.
syExpr :: P.Expr                -- ^ Input expression
          -> [Annotated D.Expr] -- ^ Output stack
          -> [String]           -- ^ Operator stack
          -> D.Sugar (Annotated D.Expr)
syExpr [] out ops =
    if null out
    then pure $ D.EId "()" :- Nothing
    else addOperators ops out
  where addOperators :: [String] -> [Annotated D.Expr] -> D.Sugar (Annotated D.Expr)
        -- ^ Adds the operators on the output stack when there is nothing left to desugar.
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
    -- ^ Adds the operator on top of the operator stack, popping other operators if needed
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
            args <- mapM (\p -> syPat [p] [] []) ss

            pure $ foldr (flip (:-) Nothing .: D.ELam) e' args
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
        P.AAnn (e :- p1) t -> do
            e' <- syExpr e [] []
            t' <- desugarType "" t

            pure $ D.EAnn e' t' :- p
        P.ALet (ps, e1 :- p1) (e2 :- p2) -> do
            ps' <- mapM (\p -> syPat [p] [] []) ps
            let (p':ps'') = ps'
            e1' <- syExpr e1 [] []
            e2' <- syExpr e2 [] []
            pure $ D.ELet (p', foldr (flip (:-) Nothing .: D.ELam) e1' ps'') e2' :- p
        P.AOperator _ -> undefined -- ! should never happen

    syExpr xs (e:out) ops

-- shunting yard for patterns

-- | Runs the Shunting Yard Algorithm on a 'P.Pattern'.
syPat :: [Annotated P.Pattern]    -- ^ Input pattern
         -> [Annotated D.Pattern] -- ^ Output stack
         -> [String]              -- ^ Operator stack
         -> D.Sugar (Annotated D.Pattern)
syPat [] out ops =
    if null out
    then pure $ D.PId "()" :- Nothing
    else addOperators out ops
  where addOperators :: [Annotated D.Pattern] -> [String] -> D.Sugar (Annotated D.Pattern)
        -- ^ Adds the operators on the output stack when there is nothing left to desugar.
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
        -- ^ Adds the operator on top of the operator stack, popping other operators if needed
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
        P.PAnn p t -> do
            p' <- syPat p [] []
            t' <- desugarType "" t

            pure $ D.PAnn p' t'
        P.PLinear p -> D.PLinear <$> syPat [p] [] []
        P.POperator _ -> undefined -- ! Should never happen

    syPat xs ((pat :- p) : out) ops