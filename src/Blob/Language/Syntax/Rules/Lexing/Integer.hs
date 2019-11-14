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

module Blob.Language.Syntax.Rules.Lexing.Integer where

import Blob.Language.Syntax.Internal.Lexing.Helpers (getPositionInSource, lexeme)
import Blob.Language.Syntax.Lexer (Lexer, currentIndent)
import Blob.Language.Syntax.Tokens.Token (Token(..))
import Blob.Language.Syntax.Tokens.Lexeme (Lexeme(LInteger))
import Control.Lens (use)
import qualified Text.Megaparsec.Char.Lexer as L

-- | This function parses an integer.
--
-- A integer currently has the following format: (it will be changed later to handle different signednesses)
--
-- > integer ::= { decimalNumber } ;
integerL :: Lexer Token
integerL = lexeme $ do
    i <- use currentIndent
    (span', int) <- getPositionInSource $
        LInteger <$> L.decimal

    pure (Token i span' (Just int))