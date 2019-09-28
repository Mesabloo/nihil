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