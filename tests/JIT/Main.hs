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

    printf <- extern "printf"
    s <- asciz "Hello, World!\n"

    mem <- allocateMemory jitsize

    let x = assemble mem (arith printf s)
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

arith :: Word32 -> Word32 -> X86 ()
arith printf msg = do
    push rbp
    mov rbp rsp
    mov rdi (Addr msg)
    call (Addr printf)
    pop rbp
    mov rax (Int 0)
    ret