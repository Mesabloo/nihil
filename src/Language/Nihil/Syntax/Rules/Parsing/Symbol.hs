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

module Language.Nihil.Syntax.Rules.Parsing.Symbol where

import Language.Nihil.Syntax.Tokens.Lexeme (Lexeme(LSymbol))
import Language.Nihil.Syntax.Parser (Parser)
import Language.Nihil.Syntax.Tokens.Token (Token(..), getLexeme)
import Text.Megaparsec (satisfy, (<?>))
import qualified Data.Text as Text
import Control.Lens ((^?), _Just, (^.), to)
import Control.Applicative (empty, liftA2)
import Data.Maybe (fromJust)
import qualified Data.Char as Ch

symbol :: String -> Parser Token
symbol s = satisfy isSym <?> ("symbol \"" <> s <> "\"")
  where
    isSym k =
        let sym = getLexeme k ^? _Just ^. to fromJust
        in case sym of
            LSymbol w
                | s == Text.unpack w -> True
            _                        -> False

opSymbol :: Parser String
opSymbol = (satisfy isOp >>= getSym) <?> "operator"
  where
    isOp s =
        let sym = getLexeme s ^? _Just ^. to fromJust
        in case sym of
            LSymbol s
                | (liftA2 (&&) isOperator (check . Text.unpack)) s -> True
            _                                                      -> False

    getSym (Token _ _ (Just (LSymbol s))) = pure (Text.unpack s)
    getSym _                              = empty

    isOperator :: Text.Text -> Bool
    isOperator = Text.all (liftA2 (||) Ch.isSymbol (`elem` "!#$%&.<=>?^~|@*/-+:"))

    check :: String -> Bool
    check x = x `notElem` rOps -- | x `elem` rOps = False -- fail ("Reserved operator \"" <> x <> "\"")
            -- | otherwise = True -- pure x

-- | The list of reserved operators in the language.
rOps :: [String]
rOps = [ "=", "::", "\\", "->", "=>", ",", "∷", "→", "⇒", "`" ]