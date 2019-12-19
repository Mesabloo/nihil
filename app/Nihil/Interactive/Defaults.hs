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

module Nihil.Interactive.Defaults where

import Nihil.Prelude (initGlobalEnv, initEvalState)
import Language.Nihil.Syntax.Internal.Desugaring.Defaults (initSugarState)
import Nihil.Interactive.REPL (REPLState(..))
import Control.Monad.State (liftIO)
import Text.PrettyPrint.ANSI.Leijen (text, bold, magenta)

-- | The default 'REPLState'.
initREPLState :: REPLState
initREPLState =
    REPLState
        initGlobalEnv
        initEvalState
        initSugarState
        "> "
        []

-- | The welcome message when starting the REPL.
initREPL :: IO ()
initREPL = liftIO $ do
    putStr ("Nihili, version " <> version <> "\t\t")
    putStr (show . bold . magenta $ text "\":?\"")
    putStrLn " for help."

-- | The current version of Nihili.
version :: String
version = "0.0.1"