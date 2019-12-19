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

module Language.Nihil.Syntax.Rules.Lexing.Keyword where

import Language.Nihil.Syntax.Tokens.Token (Token(..))
import Language.Nihil.Syntax.Tokens.Lexeme (Lexeme(LKeyword))
import Language.Nihil.Syntax.Lexer (Lexer, currentIndent)
import Language.Nihil.Syntax.Internal.Lexing.Helpers
import qualified Text.Megaparsec.Char as C
import qualified Data.Char as Ch
import Control.Lens (use)
import Control.Applicative (liftA2)
import Text.Megaparsec (choice, notFollowedBy, satisfy)
import Data.Text (Text)

-- | This function parses a keyword
keyword :: Lexer Token
keyword = lexeme $ do
    i <- use currentIndent

    (span', kw) <- getPositionInSource $
        LKeyword <$> (choice (C.string <$> kwords) <* notFollowedBy (satisfy $ liftA2 (&&) Ch.isPrint (not . Ch.isSpace)))

    pure (Token i span' (Just kw))

-- | The list of keywords of the language.
kwords :: [Text]
kwords =
    ["match", "with", "data", "type", "infixl", "infixr", "where", "let", "in"]
