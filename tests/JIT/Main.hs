{-# LANGUAGE LambdaCase, TypeApplications, RankNTypes #-}

module Main where

import Blob.Interpreter.JIT.Types
import Blob.Interpreter.JIT.ASM
import Blob.Interpreter.JIT.Compilation
import Control.Lens
import Data.Word
import Numeric

main :: IO ()
main = do
    putStrLn ""
    let jitsize = 256 * 1024 -- 256 KiB

    mem <- allocateMemory jitsize

    let x = assemble mem arith
    case x of
        Left err -> print err
        Right st -> do
            let code = st ^. mach

            dump code

            fn <- jit mem code
            res <- fn
            putStrLn ("Result: " <> show res)

            () <$ freeMemory mem jitsize

dump :: [Word8] -> IO ()
dump ls = print (showH <$> ls)
  where showH n = showHex n ""

arith :: X86 ()
arith = do
    mov rax (Int 18)
    inc rax
    inc rax
    ret