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

module Blob.Interactive.Commands.Exit where

import Blob.Interactive.Command (CommandParser, keyword, Command(..))
import System.Console.ANSI
import Text.Megaparsec (try, hidden, (<|>), (<?>))
import qualified Text.Megaparsec.Char as C
import Data.Functor (($>))
import System.Exit (exitSuccess)

-- | The 'Exit' command parser.
--
-- Either @:quit@ or @:q@.
exit :: CommandParser Command
exit = C.space *> (try . hidden) (keyword "quit" <|> keyword "q") <* C.space $> Exit <?> "߷"

exitCommand :: IO ()
exitCommand = do
    setSGR [SetColor Foreground Vivid Green]
    putStrLn "See you soon!"
    setSGR [Reset]
    exitSuccess