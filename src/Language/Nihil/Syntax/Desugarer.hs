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

module Language.Nihil.Syntax.Desugarer where

import Control.Monad.State (StateT)
import Control.Monad.Except (Except)
import Text.PrettyPrint.ANSI.Leijen (Doc)
import qualified Data.Map as Map
import qualified Language.Nihil.Syntax.Internal.Parsing.AST as P (Fixity)
import Control.Lens (makeLenses)

-- | The Desugarer monad.
type Desugarer = StateT SugarState (Except Doc)

-- | The state used in the 'Sugar' monad.
newtype SugarState
    = SugarState { _fixities :: Map.Map String P.Fixity -- ^ Contains all the declared operator fixities
                 }
makeLenses ''SugarState
