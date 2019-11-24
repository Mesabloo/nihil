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

module Blob.Language.Syntax.Tokens.Lexeme where

import Data.Text (Text, unpack)


-- | The 'Lexeme' class, which is either:
data Lexeme
    = LKeyword Text        -- ^ a keyword
    | LString Text         -- ^ a string | @"string"@
    | LInteger Integer     -- ^ an integer | @0@
    | LFloat Double        -- ^ a floating point number | @0.0@
    | LChar Char           -- ^ a character | \'c\'
    | LSymbol Text         -- ^ a symbol | most likely to be an operator
    | LLowIdentifier Text  -- ^ an identifier beginning with a lowercase letter | function or constant name
    | LUpIdentifier Text   -- ^ an identifier beginning with an uppercase letter | a type name or data type constructor
    | LWildcard            -- ^ the wildcard pattern | @_@
  deriving (Eq, Ord)

isKeyword :: Lexeme -> Bool
isKeyword (LKeyword _) = True
isKeyword            _ = False

isString :: Lexeme -> Bool
isString (LString _) = True
isString           _ = False

isInteger :: Lexeme -> Bool
isInteger (LInteger _) = True
isInteger            _ = False

isFloat :: Lexeme -> Bool
isFloat (LFloat _) = True
isFloat          _ = False

isChar :: Lexeme -> Bool
isChar (LChar _) = True
isChar         _ = False

isSymbol :: Lexeme -> Bool
isSymbol (LSymbol _) = True
isSymbol           _ = False

isLowIdentifier :: Lexeme -> Bool
isLowIdentifier (LLowIdentifier _) = True
isLowIdentifier                  _ = False

isUpIdentifier :: Lexeme -> Bool
isUpIdentifier (LUpIdentifier _) = True
isUpIdentifier                 _ = False

isWildcard :: Lexeme -> Bool
isWildcard LWildcard = True
isWildcard         _ = False

instance Show Lexeme where
    show (LKeyword w)       = "keyword " <> unpack w
    show (LString s)        = "string \"" <> unpack s <> "\""
    show (LInteger i)       = "integer " <> show i
    show (LFloat d)         = "double " <> show d
    show (LChar c)          = "character '" <> [c] <> "'"
    show (LSymbol s)        = "symbol \"" <> unpack s <> "\""
    show (LLowIdentifier i) = "identifier \"" <> unpack i <> "\""
    show (LUpIdentifier i)  = "identifier \"" <> unpack i <> "\""
    show LWildcard          = "hole"