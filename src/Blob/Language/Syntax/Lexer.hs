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

module Blob.Language.Syntax.Lexer where

import Text.Megaparsec (ParsecT)
import Data.Text (Text)
import Data.Void (Void)
import Control.Lens (makeLenses)
import Control.Monad.State (State)

-- | The 'Lexer' monad, holding a state for various information.
type Lexer = ParsecT Void Text (State LexState)

-- | The state of the 'Parser' monad.
data LexState
    = LexState { _currentIndent :: Int -- ^ the indentation level of the current line
               }

makeLenses ''LexState

-- | The default state of the lexer.
initLexState :: LexState
initLexState = LexState 0

