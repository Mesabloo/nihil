-- Blobc, a compiler for compiling Blob source code
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

module Blob.Interpreter.Evaluator where

import {-# SOURCE #-} Blob.Interpreter.Value
import Blob.Interpreter.Scope
import Control.Monad.Reader (ReaderT)
import Control.Monad.Except (ExceptT)
import Text.PrettyPrint.ANSI.Leijen (Doc)
import Control.Lens (makeLenses)

-- | The 'EvalEnv' monad used for evaluation.
type Eval = ReaderT EvalState (ExceptT EvalError IO)

-- | The error type
type EvalError = Doc

-- | The state used in the 'EvalEnv' monad.
data EvalState
    = EvalState { _vals :: Scope Value  -- ^ A mapping of all the values of all the variables known
                , _ctors :: [String]    -- ^ All the data type constructors existing in the current session
                }
makeLenses ''EvalState
