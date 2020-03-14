module Nihil.Syntax.Abstract.Desugarer.Type
( desugarType ) where

import qualified Nihil.Syntax.Concrete.Core as CC
import qualified Nihil.Syntax.Abstract.Core as AC
import Nihil.Utils.Source
import Nihil.Syntax.Common (Desugarer, typeLevelOperators)
import Nihil.Utils.Debug
import Nihil.Utils.Impossible (impossible)
import Nihil.Syntax.Abstract.Desugarer.ShuntingYard
import Nihil.Syntax.Abstract.Desugarer.Statement (desugarProgram)
import Control.Arrow ((&&&))
import Control.Lens (use)

{-| Desugars a type.

    Most types will be left untouched, except for:

    * Type applications, which are transformed into a chain of type applications.
      @t1 t2 t3 t4@ becomes @(((t1 t2) t3) t4)@/.

    * Infix operators, transformed into type applications.

    * Parenthesized types which behave like basic types.
-}
desugarType :: [CC.AType] -> Desugarer AC.Type
desugarType ty = shuntingYard ty [] []

shuntingYard :: [CC.AType] -> OperatorStack -> ValueStack AC.Type -> Desugarer AC.Type
shuntingYard [] ops []     = impossible "empty output stack"
shuntingYard [] ops out    = addOperators ops out mkApp
shuntingYard (t:ts) ops out = do
    (ops', out') <- uncurry (desugarAtom ops out) ((annotated &&& location) t)
    shuntingYard ts ops' out'

mkApp :: String -> SourcePos -> AC.Type -> AC.Type -> AC.Type
mkApp operator pos t1 t2 =
    locate (AC.TApplication (locate (AC.TApplication (locate (AC.TId operator) pos) t2) pos) t1) pos

desugarAtom :: OperatorStack -> ValueStack AC.Type -> CC.Type -> SourcePos -> Desugarer (OperatorStack, ValueStack AC.Type)
desugarAtom ops out (CC.TOperator o) pos             = do
    fixs <- use typeLevelOperators
    handleOperator o pos ops out fixs mkApp
desugarAtom ops out (CC.TId o) pos                   = pure (ops, locate (AC.TId o) pos : out)
desugarAtom ops out (CC.TVar v) pos                  = pure (ops, locate (AC.TVar v) pos : out)
desugarAtom ops out (CC.TParens t) pos               = do
    ty <- desugarType t
    pure (ops, ty : out)
desugarAtom ops out (CC.TApplication ts) pos         = do
    types <- mapM (\t -> shuntingYard [t] [] []) ts
    errorWhen (null types) "No types found for application: parsing failed" (pure ())
    let tApp = foldl1 (mkApp' pos) types
    pure (ops, tApp : out)
  where mkApp' pos t1 t2 = locate (AC.TApplication t1 t2) pos
desugarAtom ops out (CC.TTuple ts) pos               = do
    types <- mapM (\t -> shuntingYard t [] []) ts
    let tTuple = locate (AC.TTuple types) pos
    pure (ops, tTuple : out)
desugarAtom ops out (CC.TRecord row) pos      = do
    record <- AC.TRecord <$> shuntingYard [row] [] []
    pure (ops, locate record pos : out)
desugarAtom ops out (CC.TRow stts rest) pos          = do
    AC.Program ss <- desugarProgram (CC.Program stts)
    rest' <- maybe (pure Nothing) (\r -> Just <$> shuntingYard [r] [] []) rest
    pure (ops, locate (AC.TRow ss rest') pos : out)
