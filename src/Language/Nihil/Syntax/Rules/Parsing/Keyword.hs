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

module Language.Nihil.Syntax.Rules.Parsing.Keyword where

import Language.Nihil.Syntax.Tokens.Lexeme (Lexeme(LKeyword))
import Language.Nihil.Syntax.Parser (Parser)
import Language.Nihil.Syntax.Tokens.Token (Token(..), getLexeme)
import Text.Megaparsec (satisfy, (<?>))
import qualified Data.Text as Text
import Control.Lens ((^?), _Just, (^.), to)
import Data.Maybe (fromJust)

keyword :: String -> Parser Token
keyword s = satisfy isKW <?> ("keyword \"" <> s <> "\"")
  where
    isKW k =
        let t = getLexeme k ^? _Just ^. to fromJust
        in case t of
            LKeyword w
                | s == Text.unpack w -> True
            _                        -> False