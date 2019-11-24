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

module Blob.Language.Syntax.Internal.Lexing.Helpers
(lexeme, getPositionInSource, space') where

import Blob.Language.Syntax.Lexer (Lexer)
import Blob.Language.Syntax.Internal.Lexing.SourceSpan (SourceSpan(..))
import Blob.Language.Syntax.Rules.Lexing.Comment
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.Char as Ch
import Control.Monad (void)
import Text.Megaparsec (many, satisfy, some, getSourcePos)

-- | A useful function for skipping whitespaces and comments.
lexeme :: Lexer a -> Lexer a
lexeme = L.lexeme (L.space (void space1') lineCmnt blockCmnt)

-- | A Lexer for skipping many spaces (0 or more).
space' :: Lexer [Char]
space' = many (satisfy isSpace)

-- | A Lexer for skipping some spaces (1 or more).
space1' :: Lexer [Char]
space1' = some (satisfy isSpace)

-- | A simple predicate checking whether a character is a space or not.
isSpace :: Char -> Bool
isSpace c =
    let code = Ch.ord c
    in code == 9 || code == 32 || code == 160 || code == 8200 || code == 8201 || code == 8202

-- | A function used to create tokens
--
-- The 'Parser' taken as a parameter is the one used for the creation of the token.
-- It most likely returns a 'Blob.Language.Lexing.Lexeme', but not necessarily.
getPositionInSource :: Lexer a -> Lexer (SourceSpan, a)
getPositionInSource p = do
    pInit <- getSourcePos
    res   <- p
    pEnd  <- getSourcePos

    pure (SourceSpan pInit pEnd, res)
