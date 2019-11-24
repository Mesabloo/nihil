{-# LANGUAGE LambdaCase, TypeApplications, RankNTypes, BangPatterns #-}

module Main where

import Blob.Interpreter.JIT.Types
import Blob.Interpreter.JIT.ASM
import Blob.Interpreter.JIT.Compilation
import Control.Lens
import Data.Word
import Numeric
import Data.Int (Int64)
import Control.Monad (replicateM)

import Criterion.Measurement (secs, getTime)

main :: IO ()
main = do
    putStrLn ""
    let jitsize = 256 * 1024 -- 256 KiB

    -- printf <- extern "printf"
    -- s <- asciz "Hello, World!\n"

    mem <- allocateMemory jitsize

    let x = assemble mem (arith 5 8)
    case x of
        Left err -> print err
        Right st -> do
            let code = st ^. mach

            dump code

            !fn <- jit mem code
            ts <- replicateM 10000 $ time fn
            -- putStrLn ("Result: " <> show res)
            -- putStrLn ("Time taken: " <> secs t)

            let t   = map (uncurry const) ts
                min = minimum t
                max = maximum t
                avg = uncurry (/) . foldr (\e (s, c) -> (e + s, c + 1)) (0.0, 0.0) $ t
                stddev =
                    let sigma = sum $ map ((^ 2) . subtract avg) t
                    in sqrt (sigma / fromIntegral 10000)

            putStrLn $ "Results for " <> show 10000 <> " runs:"
            putStrLn $ "> Minimum: " <> secs min
            putStrLn $ "> Maximum: " <> secs max
            putStrLn $ "> Average: " <> secs avg
            putStrLn $ "> Std dev: " <> secs stddev

            () <$ freeMemory mem jitsize

dump :: [Word8] -> IO ()
dump ls = print (showH <$> ls)
  where showH n = "0x" <> showHex n ""

arith :: Int64 -> Int64 -> X86 ()
arith x y = do
    push rbp
    mov rbp rsp
    mov rax (I x)
    add rax (I y)
    pop rbp
    ret

time :: IO a -> IO (Double, a)
time f = do
    begin  <- getTime
    result <- f
    end    <- getTime
    pure $ (,) (end - begin) result