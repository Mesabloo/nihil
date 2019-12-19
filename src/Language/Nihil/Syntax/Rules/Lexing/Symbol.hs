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

module Language.Nihil.Syntax.Rules.Lexing.Symbol where

import Language.Nihil.Syntax.Internal.Lexing.Helpers (getPositionInSource, lexeme)
import Language.Nihil.Syntax.Lexer (Lexer, currentIndent)
import Language.Nihil.Syntax.Tokens.Token (Token(..))
import Language.Nihil.Syntax.Tokens.Lexeme (Lexeme(LSymbol))
import Control.Lens (use)
import Control.Applicative ((<|>))
import qualified Data.Text as Text
import qualified Text.Megaparsec.Char as C
import Text.Megaparsec (some, oneOf, (<?>))

-- | This function parses a symbol.
--
-- A symbol has the following format:
--
-- > symbol ::= '-o' | aSpecialCharacter | { symbolCharacter } ;
-- > aSpecialCharacter ::= '(' | ')' | '[' | ']' | '{' | '}' | ',' | ';' | '\\' | '→' | '⊸' | 'λ' | '⇒' | '∷'
-- > symbolCharacter ::= '!' | '#' | '$' | '%' | '&' | '.' | '<' | '=' | '>' | '?' | '^' | '~' | '|' | '@' | '*' | '/' | '-' | ':' ;
symbol :: Lexer Token
symbol = do
    i <- use currentIndent
    (span', s) <- getPositionInSource $
        (LSymbol . Text.pack . (: []) <$> lexeme (oneOf "()[]{},;\\→λ⇒∷`"))
        <|> (LSymbol . Text.pack <$> lexeme (some $ C.symbolChar <|> oneOf "!#$%&.<=>?^~|@*/-+:") <?> "symbol")

    pure (Token i span' (Just s))