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

module Blob.Language.Syntax.Rules.Parsing.Symbol where

import Blob.Language.Syntax.Tokens.Lexeme (isSymbol, Lexeme(LSymbol))
import Blob.Language.Syntax.Parser (Parser)
import Blob.Language.Syntax.Tokens.Token (Token(..), getLexeme)
import Text.Megaparsec (satisfy, (<?>))
import qualified Data.Text as Text
import Control.Lens ((^?), _Just, (^.), to)
import Control.Applicative (empty, liftA2)
import Data.Maybe (fromJust)
import qualified Data.Char as Ch

symbol :: String -> Parser Token
symbol s = (satisfy isSym >>= check) <?> ("symbol \"" <> s <> "\"")
  where
    isSym k = (isSymbol <$> getLexeme k) ^? _Just ^. to fromJust
    check t@(Token _ _ (Just (LSymbol w)))
        | s == Text.unpack w = pure t
    check _                  = empty

opSymbol :: Parser String
opSymbol = (satisfy isSym >>= getSym >>= check . Text.unpack) <?> "operator"
  where
    isSym s = (isSymbol <$> getLexeme s) ^? _Just ^. to fromJust
    getSym (Token _ _ (Just (LSymbol s)))
        | isOperator s = pure s
    getSym _           = empty

    isOperator :: Text.Text -> Bool
    isOperator = Text.all (liftA2 (||) Ch.isSymbol (`elem` "!#$%&.<=>?^~|@*/-:"))

    check :: String -> Parser String
    check x | x `elem` rOps = fail ("Reserved operator \"" <> x <> "\"")
            | otherwise = pure x

-- | The list of reserved operators in the language.
rOps :: [String]
rOps = [ "=", "::", "\\", "->", "=>", ",", "∷", "→", "⇒", "`" ]