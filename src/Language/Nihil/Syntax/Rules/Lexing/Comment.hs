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

{-# LANGUAGE OverloadedStrings #-}

module Language.Nihil.Syntax.Rules.Lexing.Comment where

import Language.Nihil.Syntax.Lexer (Lexer)
import qualified Text.Megaparsec.Char.Lexer as L

-- | A Lexer for skipping line comments.
--
-- Line comments begin with @--@.
lineCmnt :: Lexer ()
lineCmnt = L.skipLineComment "--"

-- | A Lexer for skipping block comments.
--
-- Block comments are formatted like that:
--
-- > blockCmnt ::= '{-', { anyChar } , '-}' ;
blockCmnt :: Lexer ()
blockCmnt = L.skipBlockComment "{-" "-}"
