module Blob.REPL.Logger where

import Blob.REPL.Types
import System.Console.ANSI

logError :: Show a => a -> IO ()
logError a = do
    setSGR [SetColor Foreground Vivid Red]
    print a
    setSGR [Reset]