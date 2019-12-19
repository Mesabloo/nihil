-- The Great Nihil Compiler
-- Copyright (c) 2019 Mesabloo

-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.

-- You should have received a copy of the GNU General Public License
-- along with this program. If not, see <http://www.gnu.org/licenses/>.

{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving #-}

module Language.Nihil.TypeChecking.TypeChecker where

import Language.Nihil.TypeChecking.Internal.Constraint (TypeConstraint)
import Language.Nihil.TypeChecking.Internal.Environment (GlobalEnv)
import Control.Lens (makeLenses)
import Text.PrettyPrint.ANSI.Leijen (Doc)
import Control.Monad.RWS (RWST)
import Control.Monad.Except (Except, ExceptT)
import Control.Monad.Reader (Reader)
import Control.Monad.State (StateT)

type TIError = Doc

-- | The state used in the 'Infer' monad.
newtype TypeState
    = TIState { _count :: Int  -- ^ A counter for generating new type variables
              }

makeLenses ''TypeState

-- | The inference monad
type TI = RWST
            GlobalEnv        -- ^ The typing environment
            [TypeConstraint] -- ^ The generated constraints
            TypeState        -- ^ The inference state
            (Except TIError) -- ^ The inference errors

-- | The constraint solver monad
type Solve = ExceptT TIError (Reader GlobalEnv)

-- | The type checking monad
type Check = StateT GlobalEnv (Except TIError)
