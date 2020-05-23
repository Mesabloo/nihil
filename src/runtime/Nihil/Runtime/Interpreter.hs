{-# LANGUAGE ForeignFunctionInterface #-}

module Nihil.Runtime.Interpreter
( eval ) where

import Nihil.Runtime.Core
import Foreign.Ptr (Ptr)

foreign import ccall unsafe "runtime evaluate"
    evaluate :: Ptr VExpr -> IO ()

eval :: VExpr -> IO ()
eval expr = do
    ptr <- newExpr expr
    evaluate ptr
    freeExpr ptr