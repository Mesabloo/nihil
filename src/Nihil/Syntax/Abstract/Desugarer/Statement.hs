{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Nihil.Syntax.Abstract.Desugarer.Statement
( desugarProgram ) where

import qualified Nihil.Syntax.Concrete.Core as CC
import qualified Nihil.Syntax.Abstract.Core as AC
import Nihil.Syntax.Common (Desugarer)
import Nihil.Utils.Source
import Nihil.Utils.Debug (log)
import Nihil.Utils.Impossible
import Nihil.Syntax.Abstract.Desugarer.Type (desugarType)
import Nihil.Syntax.Abstract.Desugarer.Errors.DifferentArgumentNumbers
import {-# SOURCE #-} Nihil.Syntax.Abstract.Desugarer.Expression (desugarExpression)
import Control.Arrow ((&&&))
import qualified Data.Map as Map
import Data.Bifunctor (first)
import Data.Functor ((<&>))
import Control.Monad (forM, when)
import Control.Monad.Except (throwError)
import Prelude hiding (log)

{-| Desugars a bunch of statements

    * Infix operator declarations are dropped during this process.

    * Sum types are transformed into GADTs.

    * Function definitions with parameters are transformed into lambda abstractions.
      @f x y = e@ becomes @f = λ x → λ y → e@.
-}
desugarProgram :: CC.Program -> Desugarer AC.Program
desugarProgram (CC.Program []) = pure (AC.Program [])
desugarProgram (CC.Program (s:ss)) =
    desugarStatement s ss >>= \case
        (Nothing, rem) -> desugarProgram (CC.Program rem)
        (Just x, rem)  -> desugarProgram (CC.Program rem) <&> \(AC.Program st) -> AC.Program (x:st)
-- desugarProgram (CC.Program stts) = AC.Program . catMaybes <$> mapM desugarStatement stts

desugarStatement :: CC.AStatement -> [CC.AStatement] -> Desugarer (Maybe AC.Statement, [CC.AStatement])
desugarStatement s ss =
    let (ann, pos) = (annotated &&& location) s
    in first (fmap (`locate` pos)) <$> case ann of
        CC.OperatorFixity{}             -> pure (Nothing, ss)
        CC.FunDeclaration name ty       -> do
            t <- desugarType ty
            pure (Just (AC.FunctionDeclaration name t), ss)
        CC.TypeDefinition name tvs ty   -> do
            ct <- desugarCustomType name tvs ty
            pure (Just (AC.TypeDefinition name tvs ct), ss)
        CC.FunDefinition name [] ex     -> do
            e <- desugarExpression ex
            pure (Just (AC.FunctionDefinition name e), ss)
        x@(CC.FunDefinition name ps ex) -> do
            let (es, rem) = first (locate x pos :) (span (isFun name . annotated) ss)
            forM es \e ->
                let (CC.FunDefinition _ p _) = annotated e
                in when (length p /= length ps) do
                    log (p, ps) (pure ())
                    throwError (differentNumberOfArguments (locate name (location e)) (length ps) (length p))
            let branches = fold' . annotated <$> es

            let ids = snd (foldl generateID (0, []) ps)

            (, rem) <$> desugarEPM name ids branches (location ex)
          where isFun name (CC.FunDefinition n _ _)
                    | name == n = True
                isFun _ _       = False

                fold' (CC.FunDefinition _ pats ex) = ([locate (CC.PTuple ((: []) <$> pats)) pos], ex)
                fold' _                            = impossible "Function definition for equational pattern matching are already filtered!"

                generateID (supply :: Integer, acc) pat = (supply + 1, locate (CC.PId ("#" <> show supply)) (location pat):acc)

-- | Desugaring helper for “Equational Pattern Matching”.
desugarEPM :: String -> [CC.APattern] -> [([CC.APattern], CC.AExpr)] -> SourcePos -> Desugarer (Maybe AC.Statement')
desugarEPM name ids branches pos = do
    let tuple = fold' (patToExpr <$> ids)
    ex <- desugarExpression (locate [locate (CC.ALambda ids (locate [locate (CC.AMatch tuple branches) pos] pos)) pos] pos)
    pure (Just (AC.FunctionDefinition name ex))
  where fold' toTuple = locate [locate (CC.ATuple toTuple) pos] pos

        patToExpr p = case annotated p of
            CC.PId i -> locate [locate (CC.AId i) pos] pos
            _        -> impossible "Generated identifiers for equational pattern matching are necessarily pattern identifiers."

desugarCustomType :: String -> [String] -> CC.ACustomType -> Desugarer AC.CustomType
desugarCustomType name tvs ct =
    let (ann, pos) = (annotated &&& location) ct
    in (`locate` pos) <$> desugarCustomType' name tvs pos ann

desugarCustomType' :: String -> [String] -> SourcePos -> CC.CustomType -> Desugarer AC.CustomType'
desugarCustomType' _ _ _ (CC.TypeAlias ty)         = do
    t <- desugarType ty
    pure (AC.TypeAlias t)
desugarCustomType' name tvs _ (CC.GADT ctors)      = do
    let transformed = flip Map.map ctors \c -> do
            constructor <- desugarType c
            pure (AC.Forall tvs constructor)
    AC.SumType <$> sequence transformed
desugarCustomType' name tvs pos (CC.SumType ctors) = do
    let initialType = foldl (mkApp pos) (locate (AC.TId name) pos) ((`locate` pos) . AC.TVar <$> tvs)
        concatType  = foldr (mkApp' pos) initialType
        transformed = flip Map.map ctors \c -> do
            constructor <- traverse (\c' -> desugarType [c']) c
            pure (AC.Forall tvs (concatType constructor))
    AC.SumType <$> sequence transformed
  where mkApp pos t1 t2 = locate (AC.TApplication t1 t2) pos
        mkApp' pos t1 t2 = locate (AC.TApplication (locate (AC.TApplication (locate (AC.TId "->") pos) t1) pos) t2) pos
desugarCustomType' _ tvs _ (CC.Record fields) = do
    AC.Record <$> flip traverse fields \ty -> do
        AC.Forall tvs <$> desugarType ty
