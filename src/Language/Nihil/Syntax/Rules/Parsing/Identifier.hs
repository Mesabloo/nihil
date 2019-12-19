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

module Language.Nihil.Syntax.Rules.Parsing.Identifier where

import Language.Nihil.Syntax.Parser (Parser)
import Language.Nihil.Syntax.Tokens.Lexeme (Lexeme(LUpIdentifier, LLowIdentifier), isLowIdentifier, isUpIdentifier)
import Language.Nihil.Syntax.Tokens.Token (getLexeme)
import qualified Data.Text as Text
import Text.Megaparsec (satisfy, (<?>))
import Control.Lens ((^?), (^.), to, _Just)
import Data.Maybe (fromJust)
import Control.Applicative (empty)

identifier :: Parser String
identifier = (satisfy isId >>= getId) <?> "identifier"
  where
    isId t = (isLowIdentifier <$> getLexeme t) ^? _Just ^. to fromJust
    getId t = case t ^. to getLexeme of
        Just (LLowIdentifier i) -> pure (Text.unpack i)
        _                       -> empty

typeIdentifier :: Parser String
typeIdentifier = (satisfy isId >>= getId) <?> "type identifier"
  where
    isId t = (isUpIdentifier <$> getLexeme t) ^? _Just ^. to fromJust
    getId t = case t ^. to getLexeme of
        Just (LUpIdentifier i) -> pure (Text.unpack i)
        _                      -> empty
