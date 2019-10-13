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