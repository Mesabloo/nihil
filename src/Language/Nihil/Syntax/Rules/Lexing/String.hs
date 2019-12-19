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

module Language.Nihil.Syntax.Rules.Lexing.String where

import Language.Nihil.Syntax.Internal.Lexing.Helpers (getPositionInSource, lexeme)
import Language.Nihil.Syntax.Lexer (Lexer, currentIndent)
import Language.Nihil.Syntax.Tokens.Token (Token(..))
import Language.Nihil.Syntax.Tokens.Lexeme (Lexeme(LString))
import qualified Text.Megaparsec.Char as C
import Control.Lens (use)
import Text.Megaparsec (takeWhileP)

-- | This function parses a string.
--
-- A string has the following format:
--
-- > string ::= '"', anyCharButEOL, '"' ;
stringL :: Lexer Token
stringL = lexeme $ do
    i <- use currentIndent
    (span', s) <- getPositionInSource $
        LString <$> (C.char '"' *> takeWhileP (Just "unescaped character") (/= '"') <* C.char '"')

    pure (Token i span' (Just s))