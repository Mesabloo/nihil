{-# LANGUAGE ForeignFunctionInterface #-}

module Nihil.Runtime.Interpreter
( eval ) where

import Nihil.Runtime.Core
import Foreign.Ptr (Ptr)
import Foreign.C (CInt(..), CString, newCString)
import Foreign.Marshal.Array
import Foreign.Marshal.Alloc (free)

foreign import ccall unsafe "runtime evaluate"
    evaluate :: Ptr VExpr -> CInt -> Ptr (Ptr C_Binding) -> CInt -> Ptr CString -> IO ()

eval :: VExpr -> [(String, VExpr)] -> [String] -> IO ()
eval expr binds cons = do
    let nb_binds = length binds
        nb_cons  = length cons

    ptr <- newExpr expr
    bs  <- newBindings binds
    cs  <- newCons cons

    evaluate ptr (fromIntegral nb_binds) bs (fromIntegral nb_cons) cs

    freeCons cs nb_cons
    freeBindings bs nb_binds
    freeExpr ptr

newBindings :: [(String, VExpr)] -> IO (Ptr (Ptr C_Binding))
newBindings binds = newArray =<< traverse newBinding binds

newCons :: [String] -> IO (Ptr CString)
newCons cons = newArray =<< traverse newCString cons

freeBindings :: Ptr (Ptr C_Binding) -> Int -> IO ()
freeBindings ptr n = do
    binds <- peekArray n ptr
    traverse freeBinding binds
    free ptr

freeCons :: Ptr CString -> Int -> IO ()
freeCons ptr n = do
    cons <- peekArray n ptr
    traverse free cons
    free ptr