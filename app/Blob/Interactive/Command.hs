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

{-# LANGUAGE TypeFamilies #-}

module Blob.Interactive.Command where

import Data.Void (Void)
import Text.Megaparsec (many, notFollowedBy, satisfy, some, oneOf, Parsec)
import qualified Text.Megaparsec.Char as C
import qualified Data.Char as Ch
import Control.Applicative (liftA2, (<|>))

data Command
    = GetType String
    | GetKind String
    | Help
    | Code String
    | Load String
    | Exit
    | ResetEnv [String]
    | Time String
    | Bench Integer String
    | Env
    | Shell String
  deriving (Eq, Ord, Show)

-- | A list of all existing commands.
commands :: [String]
commands =
    [  ":help", ":h", ":?"
    ,  ":quit", ":q"
    ,  ":load", ":l"
    ,  ":type", ":t"
    ,  ":kind", ":k"
    ,  ":eval", ":ev"
    , ":reset", ":r"
    ,  ":time"
    , ":bench"
    ,   ":env"
    ,     ":!" ]

-- | The command line parser.
type CommandParser = Parsec Void String

------------------------------------------------------------------

keyword :: String -> CommandParser String
keyword s = C.string s <* notFollowedBy (satisfy $ liftA2 (&&) Ch.isPrint (not . Ch.isSpace))

identifier :: CommandParser String
identifier = (:) <$> C.lowerChar <*> many (C.alphaNumChar <|> satisfy (liftA2 (||) (== '_') (== '\'')))

opSymbol :: CommandParser String
opSymbol = parens p <|> p
  where p = some $ C.symbolChar <|> oneOf "!#$%&.<=>?^~|@*/-:"
        parens p = C.char '(' *> C.space *> p <* C.space <* C.char ')'