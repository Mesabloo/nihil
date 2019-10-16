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

-- | This modules holds default values for some constants used in the REPL.
module Blob.REPL.Defaults where

import Blob.REPL.Types
import System.Console.ANSI
import System.IO
import Blob.Prelude
import Blob.Language.Desugaring.Defaults
import Control.Monad.State (liftIO)

-- | The default 'REPLState'.
initREPLState :: REPLState
initREPLState = REPLState { _ctx = initGlobalEnv
                          , _values = initEvalState
                          , _op = initSugarState
                          , _prompt = "> "
                          , _preload = [] }

-- | The welcome message when starting the REPL.
initREPL :: IO ()
initREPL = liftIO $ do
        putStr ("iBlob, version " <> version <> "\t\t") >> setSGR [Reset]
        setSGR [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Magenta] >> putStr "\":?\"" >> setSGR [Reset]
        putStrLn " for help." >> setSGR [Reset]
        hFlush stdout

-- | The current version of iBlob.
version :: String
version = "0.0.1"