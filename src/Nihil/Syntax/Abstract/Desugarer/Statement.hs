{-# LANGUAGE BlockArguments #-}

module Nihil.Syntax.Abstract.Desugarer.Statement
( desugarProgram ) where

import qualified Nihil.Syntax.Concrete.Core as CC
import qualified Nihil.Syntax.Abstract.Core as AC
import Nihil.Syntax.Common (Desugarer)
import Nihil.Utils.Source
import Nihil.Syntax.Abstract.Desugarer.Type (desugarType)
import {-# SOURCE #-} Nihil.Syntax.Abstract.Desugarer.Expression (desugarExpression)
import Control.Arrow ((&&&))
import Data.Maybe (catMaybes)
import qualified Data.Map as Map

{-| Desugars a bunch of statements

    * Infix operator declarations are dropped during this process.

    * Sum types are transformed into GADTs.

    * Function definitions with parameters are transformed into lambda abstractions.
      @f x y = e@ becomes @f = λ x → λ y → e@.
-}
desugarProgram :: CC.Program -> Desugarer AC.Program
desugarProgram (CC.Program stts) = AC.Program . catMaybes <$> mapM desugarStatement stts

desugarStatement :: CC.AStatement -> Desugarer (Maybe AC.Statement)
desugarStatement stt =
    let (ann, pos) = (annotated &&& location) stt
    in fmap (`locate` pos) <$> case ann of
        CC.OperatorFixity{} -> pure Nothing
        CC.FunDeclaration name ty -> do
            t <- desugarType ty
            pure (Just (AC.FunctionDeclaration name t))
        CC.FunDefinition name [] expr   -> do
            e <- desugarExpression expr
            pure (Just (AC.FunctionDefinition name e))
        CC.FunDefinition name pats expr -> do
            e <- desugarExpression (locate [(locate (CC.ALambda pats expr) pos)] pos)
            pure (Just (AC.FunctionDefinition name e))
        CC.TypeDefinition name tvs ty -> do
            ct <- desugarCustomType name tvs ty
            pure (Just (AC.TypeDefinition name tvs ct))

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