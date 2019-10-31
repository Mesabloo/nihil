{-# LANGUAGE LambdaCase, TemplateHaskell #-}

-- | This module holds all the data-types used in the JIT.
module Blob.Interpreter.JIT.Types where

import Data.Word
import Data.Int
import Control.Lens
import Control.Monad.State
import Control.Monad.Except

-- | A data type representing an type of value in the Assembly DSL.
data Value
    = Int Int64   -- ^ A 64-bits wide integer
    | Reg Reg     -- ^ A 'Reg'ister
    | Addr Word32 -- ^ An address
  deriving (Eq, Show)

-- | A data type to represent the primary registers in the DSL.
data Reg
    = RAX
    | RCX
    | RDX
    | RBX
    | RSP
    | RBP
    | RSI
    | RDI
  deriving (Eq, Show, Ord, Enum)

-- | A record containing useful information about the current memory
-- when generating opcodes.
data Memory
    = Memory
    { _instrs :: [Instr]  -- ^ A list of instructions to convert
    , _mach   :: [Word8]  -- ^ A list of opcodes (machine code) emitted
    , _icount :: Word32   -- ^ ?
    , _memptr :: Word32   -- ^ The base pointer for the memory
    , _memoff :: Word32   -- ^ The memory offset of the current instruction tobe inserted
                          --
                          -- In the end, it should reliably indicate how much memory to use
    }
  deriving (Eq, Show)

-- | A data-type to holds various instructions.
data Instr
    = Ret
    | Mov Value Value
    | Add Value Value
    | Sub Value Value
    | Mul Value
    | IMul Value Value
    | Xor Value Value
    | Inc Value
    | Dec Value
    | Push Value
    | Pop Value
    | Call Value
    | Loop Value
    | Nop
    | Syscall
  deriving (Eq, Show)

makeLenses ''Memory

-- | The monad where the code generation happens.
type X86 = StateT Memory (Except String)