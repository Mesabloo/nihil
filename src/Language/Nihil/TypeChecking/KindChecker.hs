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

{-# LANGUAGE TemplateHaskell #-}

module Language.Nihil.TypeChecking.KindChecker where

import Language.Nihil.TypeChecking.Internal.Constraint (KindConstraint)
import Language.Nihil.TypeChecking.Internal.Environment (KindEnv)
import Text.PrettyPrint.ANSI.Leijen (Doc)
import Control.Lens (makeLenses)
import Control.Monad.RWS (RWST)
import Control.Monad.Except (Except, ExceptT)
import Control.Monad.Reader (Reader)

-- | The error type (convenient for showing).
type KIError = Doc

-- | The state used in the kind checking.
newtype KindState
    = KIState { _kiSupply :: Int -- ^ The index for kind name generation
              }

makeLenses ''KindState

-- | The 'KI' monad is used for the kind checking.
--
-- > type KI a = ExceptT KIError (ReaderT KindEnv (State KIState)) a
type KI = RWST
            KindEnv
            [KindConstraint]
            KindState
            (Except KIError)

type Solve = ExceptT KIError (Reader KindEnv)