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

module Language.Nihil.Syntax.Rules.Lexing.Identifier where

import Language.Nihil.Syntax.Internal.Lexing.Helpers (getPositionInSource, lexeme)
import Language.Nihil.Syntax.Lexer (Lexer, currentIndent)
import Language.Nihil.Syntax.Tokens.Token (Token(..))
import Language.Nihil.Syntax.Tokens.Lexeme (Lexeme(LLowIdentifier, LUpIdentifier))
import Language.Nihil.Syntax.Rules.Lexing.Keyword (kwords)
import Control.Lens (use)
import qualified Data.Text as Text
import Text.Megaparsec (many, satisfy, (<|>))
import Control.Applicative (liftA2)
import qualified Text.Megaparsec.Char as C

-- | This function parses a lowercased identifier.
--
-- An identifier has the following format:
--
-- > identifier ::= lowerChar, { alphaNumChar | '_' | '\'' } ;
identifier :: Lexer Token
identifier = lexeme $ do
    i <- use currentIndent
    (span', ident) <- getPositionInSource $
        LLowIdentifier <$> (p >>= check)

    pure (Token i span' (Just ident))
  where p = Text.pack <$> ((:) <$> C.lowerChar <*> many (C.alphaNumChar <|> satisfy (liftA2 (||) (== '_') (== '\''))))
        check x | x `elem` kwords = fail ("“" <> Text.unpack x <> "” is a keyword and thus cannot be used as an identifier")
                | otherwise       = pure x

-- | This function parses an uppercased identifier.
--
-- An identifier has the following format:
--
-- > identifier' ::= upperChar, { alphaNumChar | '_' | '\'' } ;
identifier' :: Lexer Token
identifier' = lexeme $ do
    i <- use currentIndent
    (span', ident) <- getPositionInSource $
        LUpIdentifier . Text.pack <$> ((:) <$> C.upperChar <*> many (C.alphaNumChar <|> satisfy (liftA2 (||) (== '_') (== '\''))))

    pure (Token i span' (Just ident))