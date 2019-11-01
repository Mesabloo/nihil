{-# LANGUAGE ForeignFunctionInterface #-}

-- | Everything related to the JIT initialization is here:
-- memory allocation, opcode jitting, code assembling, etc.
module Blob.Interpreter.JIT.Compilation
( allocateMemory, freeMemory, codePtr, vecPtr
, extern, heapPtr
, getFunction, jit
, emit, imm, assemble
) where

import Data.Word
import Data.Bits
import Data.Binary.Put
import qualified Data.ByteString.Lazy.Internal as BS (unpackBytes)
import Unsafe.Coerce
import Foreign.Ptr
import Foreign.C.Types
import Foreign.Storable
import Foreign.ForeignPtr
import Foreign.Marshal.Utils (copyBytes)
import Control.Lens
import System.Posix.DynamicLinker
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as VM
import Blob.Interpreter.JIT.MMap
import Blob.Interpreter.JIT.Types
import Control.Monad.State
import Control.Monad.Except

foreign import ccall "dynamic"
  mkFun :: FunPtr (IO Int) -> IO Int

-- | A simple way to obtain a "haskell function" from a function pointer.
getFunction :: Ptr Word8 -> IO Int
getFunction mem = do
    let fptr = unsafeCoerce mem :: FunPtr (IO Int)
    mkFun fptr

-- | The heart of the JIT compiler.
--
-- Performs in two steps:
--
-- 1. Copies all the opcodes given to the address of the pointer also given
--
-- 2. Returns a function with 'getFunction', from the pointer given.
jit :: Ptr Word8 -> [Word8] -> IO (IO Int)
jit mem machCode = do
    code <- codePtr machCode
    withForeignPtr (vecPtr code) $
        \ptr -> copyBytes mem ptr (length machCode)

    pure (getFunction mem)

-- | This function converts a single opcode list into a vector of opcodes.
codePtr :: [Word8] -> IO (VM.IOVector Word8)
codePtr = V.thaw . V.fromList

-- | This function converts a vector of opcodes into a 'ForeignPtr'.
vecPtr :: Storable a => VM.MVector s a -> ForeignPtr a
vecPtr = fst . VM.unsafeToForeignPtr0

-- | Allocates a chunk of fixed size and returns a pointer to the beginning of the chunk.
allocateMemory :: CSize -> IO (Ptr Word8)
allocateMemory size = mmap nullPtr size pflags mflags (-1) 0
  where
    pflags = protRead <> protWrite
    mflags = mmapAnon .|. mmapPrivate

-- | Frees a chunk of memory given its base pointer and its size.
freeMemory :: Ptr Word8 -> CSize -> IO Int
freeMemory = munmap

-- | Returns the address of an external function to be called.
extern :: String -> IO Word32
extern name = do
    dl <- dlopen "" [RTLD_LAZY, RTLD_GLOBAL]
    fn <- dlsym dl name
    pure (heapPtr $ castFunPtrToPtr fn)

-- | Returns the address held by a pointer.
heapPtr :: Ptr a -> Word32
heapPtr = fromIntegral . ptrToIntPtr

-- | Assembles a opcode sequence from a base pointer and a program.
assemble :: Ptr a -> X86 b -> Either String Memory
assemble start = runExcept . flip execStateT (istate ptr)
  where ptr = heapPtr start

        istate start = Memory [] [] 0 start start

---------------------------------------------------------

-- | Emits some opcodes into the opcode sequence held by the state.
emit :: [Word8] -> X86 ()
emit i = do
    mach %= (<> i)
    memoff += fromIntegral (length i)

-- | Emits opcodes given an immediate value.
imm :: Integral a => a -> X86 ()
imm = emit . toBytes

-- | Transforms an integral value into a little endian representation of its bytes.
toBytes :: Integral a => a -> [Word8]
toBytes x = BS.unpackBytes bs
  where bs = runPut $ putWord32le (fromIntegral x)
