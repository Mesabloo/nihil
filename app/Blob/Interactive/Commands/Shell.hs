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

{-# LANGUAGE LambdaCase #-}

module Blob.Interactive.Commands.Shell where

import Blob.Interactive.Command (CommandParser, keyword, Command(..))
import Blob.Interactive.REPL (REPL)
import Text.Megaparsec (try, hidden, (<?>), observing, lookAhead, anySingle, someTill, eof)
import qualified Text.Megaparsec.Char as C
import Control.Monad.State (liftIO)
import Control.Exception (handle, SomeException)
import System.Process (callCommand)

-- | The 'Shell' command parser.
--
-- @:!@
shell :: CommandParser Command
shell = do
    C.space *> (try . hidden) (keyword "!") <* C.space <?> "߷"

    observing (lookAhead eof) >>= \case
        Right _ -> fail "Missing argument \"[command]\""
        Left _  -> Shell <$> (anySingle `someTill` eof)

shell' :: String -> REPL ()
shell' = liftIO . handle handleException . callCommand
  where handleException :: SomeException -> IO ()
        handleException _ = pure ()