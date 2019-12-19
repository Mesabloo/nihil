{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable #-}

-- | This module provides acces to the @sys/mman.h@ library in C.
--
-- It is however not portable as of today, not handling Windows.
--
-- See @man mmap@ and @man munmap@ for more information/documentation.
module Nihil.Interpreter.JIT.MMap
( ProtOption, MmapOption
, protExec, protRead, protWrite
, mmapAnon, mmapPrivate, mmapNone
, MmapException
, mmap, munmap
) where

import Data.Bits
import Data.Typeable
import Data.Word

import Control.Exception
import Control.Monad (when)

import Foreign.C.Types
import Foreign.Ptr

import System.Posix.Types

-- | The flags used in the module
mapAnon, mapPrivate, mapNone :: MmapFlags
mapAnon    = 0x20
mapPrivate = 0x02
mapNone    = 0x0

-- | A type representing flags for the 'mmap' function.
newtype MmapFlags = MmapFlags { unMmapFlags :: CInt }
  deriving (Eq, Show, Ord, Num, Bits)

--------------------------------------------------------------

-- | A type holding the page protection options.
newtype ProtOption = ProtOption CInt
  deriving (Eq, Show, Ord, Num, Bits)

-- | A type holding the mapping options.
newtype MmapOption = MmapOption CInt
  deriving (Eq, Show, Ord, Num, Bits)

-- | Pages can be executed.
protExec :: ProtOption
protExec = ProtOption 0x01

-- | Pages can be read.
protRead :: ProtOption
protRead = ProtOption 0x04

-- | Pages can be written.
protWrite :: ProtOption
protWrite = ProtOption 0x02

-- | No page protection.
protNone :: ProtOption
protNone = ProtOption 0x0

-- | No mapping.
mmapNone :: MmapOption
mmapNone = MmapOption (unMmapFlags mapNone)

-- | Anonymous mapping.
mmapAnon :: MmapOption
mmapAnon = MmapOption (unMmapFlags mapAnon)

-- | Private mapping.
mmapPrivate :: MmapOption
mmapPrivate = MmapOption (unMmapFlags mapPrivate)

instance Monoid ProtOption where
    mempty = protNone

instance Semigroup ProtOption where
    (<>) (ProtOption a) (ProtOption b) = ProtOption (a .|. b)

instance Monoid MmapOption where
    mempty = mmapNone

instance Semigroup MmapOption where
  (<>) (MmapOption a) (MmapOption b) = MmapOption (a .|. b)

----------------------------------------------------------------------------

-- | A simple way to represent an exception occurring when performing
-- a @mmap@ computation.
data MmapException = MmapException
  deriving (Eq, Ord, Show, Typeable)

instance Exception MmapException

----------------------------------------------------------------------------

foreign import ccall unsafe "sys/mman.h mmap"
    c_mmap :: Ptr () -> CSize -> ProtOption -> MmapOption -> Fd -> COff -> IO (Ptr a)
foreign import ccall unsafe "sys/mman.h munmap"
    c_munmap :: Ptr a -> CSize -> IO Int

-- | The 'mmap' function is used to allocate pages.
mmap :: Ptr () -> CSize -> ProtOption -> MmapOption -> Fd -> COff -> IO (Ptr Word8)
mmap addr len prot flags fd offset = do
    ptr <- c_mmap addr len prot flags fd offset
    when (ptr == intPtrToPtr (-1)) $ throwIO MmapException
    pure ptr

-- | The 'munmap' function is used to free pages.
munmap :: Ptr a -> CSize -> IO Int
munmap addr len = do
    ret <- c_munmap addr len
    when (ret == -1) $ throwIO MmapException
    pure ret