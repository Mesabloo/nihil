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

module Language.Nihil.Syntax.Rules.Lexing.Program
(tokens) where

import Language.Nihil.Syntax.Lexer (Lexer, currentIndent)
import Language.Nihil.Syntax.Tokens.Token (Token(..))
import Language.Nihil.Syntax.Internal.Lexing.Helpers
import Language.Nihil.Syntax.Rules.Lexing.Keyword
import Language.Nihil.Syntax.Rules.Lexing.Comment
import Language.Nihil.Syntax.Rules.Lexing.String
import Language.Nihil.Syntax.Rules.Lexing.Integer
import Language.Nihil.Syntax.Rules.Lexing.Float
import Language.Nihil.Syntax.Rules.Lexing.Char
import Language.Nihil.Syntax.Rules.Lexing.Symbol
import Language.Nihil.Syntax.Rules.Lexing.Identifier
import Language.Nihil.Syntax.Rules.Lexing.Wildcard
import Control.Lens (use, (.=))
import Text.Megaparsec (optional, try, (<|>), choice, many, satisfy, eof)
import qualified Data.Char as Ch

-- | The main function, used to transform a source file into a list of tokens.
tokens :: Lexer [Token]
tokens = lexeme (indent *> optional (try blockCmnt <|> lineCmnt) *> many tks) <* eof
  where tks = lexeme $ choice
            [ try keyword
            , stringL
            , try floatL
            , integerL
            , charL
            , symbol
            , identifier
            , identifier'
            , eolI
            , wildcard ]

-- | This function is used to calculate the indentation level of each line when EOL is encountered.
eolI :: Lexer Token
eolI = do
    (span', _) <- getPositionInSource $ do
        satisfy isEndOfLine
        indent

    i <- use currentIndent
    pure (Token i span' Nothing)
  where
    -- | A simple predicate checking whether a character is an end of line character or not.
    isEndOfLine :: Char -> Bool
    isEndOfLine c =
        let code = Ch.ord c
        in code == 10 || code == 11 || code == 12 || code == 13 || code == 133 || code == 8232 || code == 8233


-- | The internal indentation calculator used in @eolI@.
indent :: Lexer ()
indent = do
    indent' <- length <$> space'
    currentIndent .= indent'