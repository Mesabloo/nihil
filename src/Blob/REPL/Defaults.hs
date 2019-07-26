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
                          , lastExecTime = 0.0
                          , op = initSugarState }

initREPL :: IO ()
initREPL = liftIO $ do
        setSGR  [SetColor Foreground Vivid White] >> putStr ("iBlob v" <> version <> "\nType ") >> setSGR [Reset]
        setSGR [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Magenta] >> putStr "“:?”" >> setSGR [Reset]
        setSGR  [SetColor Foreground Vivid White] >> putStrLn " for a list of commands." >> setSGR [Reset]
        hFlush stdout

version :: String
version = "0.0.1"