-- iBlob, a REPL using the Blob programming language's interpreter.
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

-- | This module holds some functions for logging stuff.
--
-- It should be either completed or deleted.
module Blob.REPL.Logger where

import Blob.REPL.Types
import System.Console.ANSI

-- | Shows a message in red.
logError :: Show a => a -> IO ()
logError a = do
    setSGR [SetColor Foreground Vivid Red]
    putStr (show a)
    setSGR [Reset]