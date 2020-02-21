{-# LANGUAGE TemplateHaskell #-}

module Nihil.TypeChecking.Common
( -- * General
  Solve
, Infer
, InferState(IState)
, supply
  -- * Types
, SolveType
, InferType
, TypeCheck
, TCConstraints(TCConstraints)
, typeConstraint
, classConstraint
  -- * Kinds
, SolveKind
, InferKind
) where

import Nihil.TypeChecking.Environment
import Nihil.TypeChecking.Constraint
import Control.Monad.Except (ExceptT, Except)
import Control.Monad.Reader (Reader)
import Control.Monad.RWS (RWST)
import Control.Monad.State (StateT)
import Text.PrettyPrint.ANSI.Leijen (Doc)
import Control.Lens (makeLenses)

type Solve e a = ExceptT Doc (Reader e) a
type SolveType a = Solve GlobalEnv a
type SolveKind a = Solve KindEnv a

data InferState
    = IState
    { _supply :: Int    -- ^ Supply count; incremented each time a new variable is created.
    }
makeLenses ''InferState

data TCConstraints
    = TCConstraints
    { _typeConstraint  :: [TypeConstraint]
    , _classConstraint :: [ClassConstraint]
    }
  deriving Show
makeLenses ''TCConstraints

instance Semigroup TCConstraints where
    TCConstraints tc1 cc1 <> TCConstraints tc2 cc2 = TCConstraints (tc1 <> tc2) (cc1 <> cc2)

instance Monoid TCConstraints where
    mempty = TCConstraints mempty mempty

type Infer e cs a = RWST e cs InferState (Except Doc) a
type InferType a = Infer GlobalEnv TCConstraints a
type InferKind a = Infer KindEnv [KindConstraint] a

type TypeCheck a = StateT GlobalEnv (Except Doc) a