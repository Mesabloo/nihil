module Blob.REPL.Defaults where

import Blob.REPL.Types
import System.Console.ANSI
import System.IO
import Blob.Prelude
import Blob.Language.Desugaring.Defaults
import Control.Monad.State (liftIO)

initREPLState :: REPLState
initREPLState = REPLState { ctx = initGlobalEnv
                          , values = initEvalState
                          , op = initSugarState
                          , prompt = "> " }

initREPL :: IO ()
initREPL = liftIO $ do
        putStr ("iBlob, version " <> version <> "\t") >> setSGR [Reset]
        setSGR [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Magenta] >> putStr "\":?\"" >> setSGR [Reset]
        putStrLn " for help." >> setSGR [Reset]
        hFlush stdout

version :: String
version = "0.0.1"