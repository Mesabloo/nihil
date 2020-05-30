{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}

{-| The accumulation process is the process of registering every operator used (type operators, expression operators, etc)
    in order to desugar types, expressions and patterns correctly.

    Statements are divided in 2 parts:

    * Operator fixity declarations

    * Others

    They are then processed as follows:

    * Fixity declarations are handled at first by just adding the operator to the map, along with its complete fixity
      (associativity and precedence)

    * Statements are then processed in the order of their appearance in the source code:

        * When a type is encountered, any operator is registered /only if there isn't already an operator in the map/.

        * When an expression is encountered, the same operation is applied.

        * And same goes for patterns.
-}
module Nihil.Syntax.Abstract.Accumulator
( accumulateOnProgram ) where

import Nihil.Syntax.Concrete.Core
import Nihil.Utils.Source (annotated)
import Nihil.Syntax.Common (Desugarer, valueLevelOperators, patternLevelOperators, typeLevelOperators)
import Data.List (partition)
import qualified Data.Map as Map
import Control.Lens ((%=))
import Data.Foldable (traverse_, forM_)
import Control.Arrow ((<<<), (>>>))

{-| Accumulator function on a given “sugared” program.

    It first splits the statements into two parts:

    * Operator fixit declarations

    * Other statements

    Those statements are then reordered in order to analyse fixity declarations first.
-}
accumulateOnProgram :: Program -> Desugarer ()
accumulateOnProgram (Program stts) =
    let (ops, others) = partition isOperatorFixityStatement (annotated <$> stts)
    in mapM_ accumulateOnStatement (ops <> others)
  where isOperatorFixityStatement OperatorFixity{} = True
        isOperatorFixityStatement _                = False

accumulateOnStatement :: Statement -> Desugarer ()
accumulateOnStatement (OperatorFixity name afix) = do
    let (Infix assoc prec) = annotated afix
    valueLevelOperators   %= Map.insert name (assoc, prec)
    typeLevelOperators    %= Map.insert name (assoc, prec)
    patternLevelOperators %= Map.insert name (assoc, prec)
accumulateOnStatement (FunDeclaration _ ty) =
    accumulateOnType ty
accumulateOnStatement (FunDefinition _ pats ex) = do
    accumulateOnPattern pats
    accumulateOnExpression ex
accumulateOnStatement (TypeDefinition _ _ ct) = do
    let ty = annotated ct
    accumulateOnCustomType ty

accumulateOnCustomType :: CustomType -> Desugarer ()
accumulateOnCustomType (TypeAlias ty) = accumulateOnType ty
accumulateOnCustomType (GADT ctors) =
    traverse_ accumulateOnType ctors
accumulateOnCustomType (SumType ctors) =
    traverse_ accumulateOnType ctors

accumulateOnType :: [AType] -> Desugarer ()
accumulateOnType = mapM_ (accumulateOnType' <<< annotated)
  where accumulateOnType' :: Type -> Desugarer ()
        accumulateOnType' (TOperator name)  =
            typeLevelOperators %= Map.insertWith (flip const) name (L, 9)
        accumulateOnType' (TApplication ts) =
            accumulateOnType ts
        accumulateOnType' (TParens t)       =
            accumulateOnType t
        accumulateOnType' (TTuple ts)       =
            mapM_ accumulateOnType ts
        accumulateOnType' (TRow stts _)     =
            accumulateOnProgram (Program stts)
        accumulateOnType' (TRecord row)     =
            accumulateOnType [row]
        accumulateOnType' _                 = pure ()

accumulateOnPattern :: [APattern] -> Desugarer ()
accumulateOnPattern = mapM_ (accumulateOnPattern' <<< annotated)
  where accumulateOnPattern' :: Pattern -> Desugarer ()
        accumulateOnPattern' (POperator name)      =
            patternLevelOperators %= Map.insertWith (flip const) name (L, 9)
        accumulateOnPattern' (PConstructor _ pats) =
            accumulateOnPattern pats
        accumulateOnPattern' (PParens p)           =
            accumulateOnPattern p
        accumulateOnPattern' (PTuple pats)         =
            mapM_ accumulateOnPattern pats
        accumulateOnPattern' (PTypeAnnotated p ty) = do
            accumulateOnType ty
            accumulateOnPattern p
        accumulateOnPattern' _                     = pure ()

accumulateOnExpression :: Expr -> Desugarer ()
accumulateOnExpression = mapM_ accumulateOnExpression'
  where accumulateOnExpression' :: AAtom -> Desugarer ()
        accumulateOnExpression' = annotated >>> \case
            AOperator name       ->
                valueLevelOperators %= Map.insertWith (flip const) name (L, 9)
            ATuple es            ->
                mapM_ accumulateOnExpression es
            ALambda pats expr    -> do
                accumulateOnPattern pats
                accumulateOnExpression expr
            AMatch expr branches -> do
                accumulateOnExpression expr
                forM_ branches \(p, e) -> do
                    accumulateOnPattern p
                    accumulateOnExpression e
            AParens e            ->
                accumulateOnExpression e
            AApplication expr    ->
                mapM_ accumulateOnExpression' expr
            ATypeAnnotated e ty  -> do
                accumulateOnType ty
                accumulateOnExpression e
            ALet stts ex         -> do
                accumulateOnProgram (Program stts)
                accumulateOnExpression ex
            AWhere ex stts       -> do
                accumulateOnExpression ex
                accumulateOnProgram (Program stts)
            ARecord stts         ->
                accumulateOnProgram (Program stts)
            ARecordAccess ex _   ->
                accumulateOnExpression [ex]
            _                    -> pure ()
