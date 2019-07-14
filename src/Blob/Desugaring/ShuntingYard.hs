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

syExpr :: P.Expr     -- ^ Input expression
          -> D.SY ()
syExpr [] = do
    ops <- gets D.operators
    out <- gets D.output

    if null out
    then modify $ \st -> st { D.output = (D.EId "()" :- Nothing) : D.output st }
    else addOperators
  where addOperators :: D.SY ()
        addOperators = do
            ops <- gets D.operators

            forM_ ops $ \o -> do
                out <- gets D.output

                if length out < 2
                then throwError $ makeUnexpectedOperatorError o
                else let (e1:e2:es) = out
                     in modify $ \st -> st { D.output = (D.EApp (D.EApp (D.EId o :- Nothing) e2 :- Nothing) e1 :- Nothing) : es }
syExpr ((P.AOperator o :- _):xs) = do
    handleOperator o

    syExpr xs
    where handleOperator :: String -> D.SY ()
          handleOperator o = do
            ops <- gets D.operators
            ops' <- gets D.opFixities

            if null ops
            then modify $ \st -> st { D.operators = [o] }
            else
                let (o1:os) = ops
                    P.Infix assoc1 prec1 _ = ops' Map.! o1
                    P.Infix _ prec2 _ = ops' Map.! o
                in  if (prec1 > prec2 || (prec1 == prec2 && assoc1 == P.L)) && o1 /= "("
                    then do
                        es <- gets D.output

                        if length es < 2
                        then throwError $ makeUnexpectedOperatorError o1
                        else do
                            let (e1:e2:es') = es
                            modify $ \st -> st { D.output = (D.EApp (D.EApp (D.EId o1 :- Nothing) e2 :- Nothing) e1 :- Nothing) : es'
                                                , D.operators = os }
                            handleOperator o
                    else modify $ \st -> st { D.operators = o : D.operators st }
syExpr ((x :- p):xs) = do
    ops' <- gets D.opFixities

    e <- case x of
        P.AId id' -> pure (D.EId id' :- p)
        P.AHole -> pure (D.EHole :- p)
        P.ALit (P.LStr s) -> pure $ foldr (\t acc -> D.EApp (D.EApp (D.EId ":" :- Nothing) (D.ELit (D.LChr t) :- Nothing) :- Nothing) (acc :- Nothing)) (D.EId "[]") s :- p
        P.ALit (P.LChr c) -> pure $ D.ELit (D.LChr c) :- p
        P.ALit (P.LInt i) -> pure $ D.ELit (D.LInt i) :- p
        P.ALit (P.LDec d) -> pure $ D.ELit (D.LDec d) :- p
        P.AApp a1 a2 -> do
            let res1 = runExcept $ runStateT (syExpr [a1]) (initSYState ops')
                res2 = runExcept $ runStateT (syExpr [a2]) (initSYState ops')
            e2 <- case res1 of
                Left err -> throwError err
                Right (_, st1) -> pure . head $ D.output st1
            e1 <- case res2 of
                Left err -> throwError err
                Right (_, st2) -> pure . head $ D.output st2

            pure (D.EApp e2 e1 :- p)
        P.AParens (a :- _) -> do
            let res = runExcept $ runStateT (syExpr a) (initSYState ops')
            case res of
                Left err -> throwError err
                Right (_, st) -> do
                    let e1 = head $ D.output st

                    pure e1
        P.ALambda ss (e :- p1) -> do
            let res = runExcept $ runStateT (syExpr e) (initSYState ops')
            case res of
                Left err -> throwError err
                Right (_, st) -> do
                    let e1 = head $ D.output st

                    pure $ foldr (flip (:-) Nothing .: D.ELam) e1 ss
        P.ATuple es -> do
            let es' = map getAnnotated es
            let res = mapM (\e -> runExcept $ runStateT (syExpr e) (initSYState ops')) es'
            case res of
                Left err -> throwError err
                Right result -> do
                    let es''' = map (head . D.output . snd) result
            
                    pure $ D.ETuple (reverse es''') :- p
        P.AList es -> do
            let es' = map getAnnotated es
            let res = mapM (\e -> runExcept $ runStateT (syExpr e) (initSYState ops')) es'
            case res of
                Left err -> throwError err
                Right result -> do
                    let es''' = map (head . D.output . snd) result

                    pure $ getAnnotated (foldr (\t acc -> D.EApp (D.EApp (D.EId ":" :- Nothing) t :- Nothing) acc :- Nothing) (D.EId "[]" :- Nothing) es''') :- p
        P.AMatch (e :- p1) cases -> do
            let res = runExcept $ runStateT (syExpr e) (initSYState ops')
            toMatch <- case res of
                Left err -> throwError err
                Right (_, st) -> pure . head $ D.output st

            branches <- mapM (uncurry handleBranch) cases 

            pure $ D.EMatch toMatch branches :- p
          where
            handleBranch :: [Annotated P.Pattern] -> Annotated P.Expr -> D.SY (Annotated D.Pattern, Annotated D.Expr)
            handleBranch [] _ = undefined
            handleBranch pats (branch :- p1) = do
                ops' <- gets D.opFixities

                let p = runExcept $ runStateT (syPat pats) (initSYPatState ops')

                p' <- case p of
                    Left err -> throwError err
                    Right (_, pattern') -> pure pattern'

                let res = runExcept $ runStateT (syExpr branch) (initSYState ops')
                case res of
                    Left err -> throwError err
                    Right (_, st) -> do
                        let e1 = head $ D.output st

                        pure (head $ D.outputP p', e1)
        P.AOperator _ -> undefined -- ! should never happen


    modify $ \st -> st { D.output = e : D.output st }

    syExpr xs

-- shunting yard for patterns

syPat :: [Annotated P.Pattern] -- ^ Input pattern
         -> D.SYPat ()
syPat [] = do
    ops <- gets D.operatorsP
    out <- gets D.outputP

    if null out
    then pure ()
    else addOperatorsP
  where addOperatorsP :: D.SYPat ()
        addOperatorsP = do
            ops <- gets D.operatorsP

            forM_ ops $ \o -> do
                out <- gets D.outputP

                if length out < 2
                then throwError $ makeUnexpectedOperatorError o
                else let (e1:e2:es) = out
                     in modify $ \st -> st { D.outputP = (D.PCtor o [e2, e1] :- Nothing) : es }
syPat ((P.POperator o :- p):xs) = do
    handleOperator o

    syPat xs
    where handleOperator :: String -> D.SYPat ()
          handleOperator o = do
            ops <- gets D.operatorsP
            ops' <- gets D.opFixitiesP

            if null ops
            then modify $ \st -> st { D.operatorsP = [o] }
            else
                let (o1:os) = ops
                    P.Infix assoc1 prec1 _ = ops' Map.! o1
                    P.Infix _ prec2 _ = ops' Map.! o
                in  if (prec1 > prec2 || (prec1 == prec2 && assoc1 == P.L)) && o1 /= "("
                    then do
                        es <- gets D.outputP

                        if length es < 2
                        then throwError $ makeUnexpectedOperatorError o1
                        else do
                            let (e1:e2:es') = es
                            modify $ \st -> st { D.outputP = (D.PCtor o1 [e2, e1] :- Nothing) : es'
                                                , D.operatorsP = os }
                            handleOperator o
                    else modify $ \st -> st { D.operatorsP = o : D.operatorsP st }
syPat ((x :- p):xs) = do
    pat <- case x of
        P.PHole -> pure D.Wildcard
        P.PId id' -> pure $ D.PId id'
        P.PLit (P.LStr s) -> pure $ foldr (\t acc -> D.PCtor ":" [D.PChr t :- Nothing, acc :- Nothing]) (D.PCtor "[]" []) s
        P.PLit (P.LChr c) -> pure $ D.PChr c
        P.PLit (P.LInt i) -> pure $ D.PInt i
        P.PLit (P.LDec d) -> pure $ D.PDec d
        P.PCtor id' pats -> do
            ops' <- gets D.opFixitiesP
            let r = forM pats $ \pat -> runExcept $ runStateT (syPat pat) (initSYPatState ops')
            case r of
                Left err -> throwError err
                Right result -> pure . D.PCtor id' $ map (head . D.outputP . snd) result
        P.PTuple pats -> do
            ops' <- gets D.opFixitiesP
            let r = forM pats $ \pat -> runExcept $ runStateT (syPat pat) (initSYPatState ops')
            case r of
                Left err -> throwError err
                Right result -> pure . D.PTuple $ map (head . D.outputP . snd) result
        P.PParens pat -> do
            ops' <- gets D.opFixitiesP
            let r = runExcept $ runStateT (syPat pat) (initSYPatState ops')
            case r of
                Left err -> throwError err
                Right (_, result) -> pure . getAnnotated . head . D.outputP $ result
        P.PList es -> do
            ops' <- gets D.opFixitiesP
            let r = forM es $ \pat -> runExcept $ runStateT (syPat pat) (initSYPatState ops')
            case r of
                Left err -> throwError err
                Right result ->
                    let es = map (head . D.outputP . snd) result
                    in pure $ getAnnotated (foldr (\t acc -> D.PCtor ":" [t, acc] :- Nothing) (D.PCtor "[]" [] :- Nothing) es)
        P.POperator _ -> undefined -- ! Should never happen

    modify $ \st -> st { D.outputP = (pat :- p) : D.outputP st }

    syPat xs