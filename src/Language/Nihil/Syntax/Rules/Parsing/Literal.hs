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

module Language.Nihil.Syntax.Rules.Parsing.Literal where

import Language.Nihil.Syntax.Tokens.Lexeme
import Language.Nihil.Syntax.Parser (Parser)
import Language.Nihil.Syntax.Tokens.Token (Token(..), getLexeme)
import Control.Lens ((^?), (^.), to, _Just)
import Control.Applicative (empty)
import Text.Megaparsec (satisfy, (<?>))
import Data.Maybe (fromJust)
import qualified Data.Text as Text

integer :: Parser Integer
integer = (satisfy isInt >>= getInt) <?> "integer"
  where
    isInt t = (isInteger <$> getLexeme t) ^? _Just ^. to fromJust

    getInt (Token _ _ (Just (LInteger i))) = pure i
    getInt _                               = empty

float :: Parser Double
float = (satisfy isFloat' >>= getFloat) <?> "floating point number"
  where
    isFloat' t = (isFloat <$> getLexeme t) ^? _Just ^. to fromJust

    getFloat (Token _ _ (Just (LFloat f))) = pure f
    getFloat _                             = empty

char :: Parser Char
char = (satisfy isChr >>= getChr) <?> "character"
  where
    isChr t = (isChar <$> getLexeme t) ^? _Just ^. to fromJust

    getChr (Token _ _ (Just (LChar c))) = pure c
    getChr _                            = empty

string :: Parser String
string = (satisfy isStr >>= getString) <?> "string"
  where
    isStr t = (isString <$> getLexeme t) ^? _Just ^. to fromJust

    getString (Token _ _ (Just (LString s))) = pure (Text.unpack s)
    getString _                              = empty