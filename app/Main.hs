{-# LANGUAGE LambdaCase #-}

module Main 
(
    main
) where

import Blob.REPL.REPL

main :: IO (Either Blob.REPL.REPL.REPLError ())
main = runREPL replLoop