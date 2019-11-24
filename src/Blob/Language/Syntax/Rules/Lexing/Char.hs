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

module Blob.Language.Syntax.Rules.Lexing.Char where

import Blob.Language.Syntax.Internal.Lexing.Helpers (getPositionInSource, lexeme)
import Blob.Language.Syntax.Lexer (Lexer, currentIndent)
import Blob.Language.Syntax.Tokens.Token (Token(..))
import Blob.Language.Syntax.Tokens.Lexeme (Lexeme(LChar))
import Control.Lens (use)
import qualified Text.Megaparsec.Char as C
import Text.Megaparsec (anySingle)

-- | This function parses a character.
--
-- A character has the following format:
--
-- > character ::= '\'', anyCharButEOL, '\'' ;
charL :: Lexer Token
charL = lexeme $ do
    i <- use currentIndent
    (span', chr) <- getPositionInSource $
        LChar <$> (C.char '\'' *> anySingle <* C.char '\'')

    pure (Token i span' (Just chr))