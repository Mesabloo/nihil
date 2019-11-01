{-# LANGUAGE TemplateHaskell #-}

-- | This module holds all the assembly DSL instructions.
--
-- More information at
-- https://www.intel.com/content/dam/www/public/us/en/documents/manuals/64-ia-32-architectures-software-developer-instruction-set-reference-manual-325383.pdf
module Blob.Interpreter.JIT.ASM where

import Blob.Interpreter.JIT.TH
import Blob.Interpreter.JIT.Types
import Blob.Interpreter.JIT.Compilation
import Data.Bits
import Control.Monad.Except
import Data.Word
import Control.Lens hiding (index)
import Foreign.C.String

-- https://www.felixcloutier.com/x86/

$(mkRegFuns)

-- | Instruction prefix used for widening the instruction size (mostly to handle x86-64 instructions).
rexW :: Word8
rexW = 0x48

-- | The ModR/M byte mostly used in this DSL.
modRM :: Word8
modRM = 0xC0

---------------------------------------------------------------------------------------------

ret :: X86 ()
ret =
    emit [ 0xc3 ]

add :: Value -> Value -> X86 ()
add (Reg l) (Int r) = do
    emit [ rexW
         , 0x05 ]              -- ADD
    imm r
add (Reg l) (Reg r) =
    emit [ rexW
         , 0x01                -- ADD
         , modRM .|. index r `shiftL` 3 .|. index l ]
add _ _ = nodef

sub :: Value -> Value -> X86 ()
sub (Reg l) (Int r) = do
    emit [ rexW
         , 0x2D ]              -- SUB
    imm r
sub (Reg l) (Reg r) =
    emit [ rexW
         , 0x29                -- SUB
         , modRM .|. index r `shiftL` 3 .|. index l]
sub _ _ = nodef

push :: Value -> X86 ()
push (Reg l) =
    emit [ 0x50 + index l ]
push _ = nodef

pop :: Value -> X86 ()
pop (Reg l) =
    emit [ 0x58 + index l ]
pop _ = nodef

call :: Value -> X86 ()
call (Addr dst) = do
    src <- use memoff
    emit [ 0xE8 ]
    imm (dst - (src + 5))
call _ = nodef

mul :: Value -> X86 ()
mul (Reg l) =
    emit [ rexW
         , 0xF7                -- MUL
         , 0xE0 .|. index l ]
mul _ = nodef

imul :: Value -> Value -> X86 ()
imul (Reg l) (Int r) = do
    emit [ rexW
         , 0x69                -- IMUL
         , modRM .|. index l ]
    imm r
imul (Reg l) (Reg r) =
    emit [ rexW
         , 0x0F
         , 0xAF                -- IMUL
         , modRM .|. index r `shiftL` 3 .|. index l ]
imul _ _ = nodef

mov :: Value -> Value -> X86 ()
mov (Reg dst) (Int src) = do
    emit [ rexW
         , 0xC7                -- MOV
         , modRM .|. (index dst .&. 7) ]
    imm src
mov (Reg dst) (Addr src) = do
    emit [ rexW
         , 0xC7                -- MOV
         , 0xC7 ]
    imm src
mov (Reg dst) (Reg src) =
    emit [ rexW
         , 0x89                -- MOV
         , modRM .|. index src `shiftL` 3 .|. index dst ]
mov _ _ = nodef

nop :: X86 ()
nop =
    emit [ 0x90 ]              -- NOP

inc :: Value -> X86 ()
inc (Reg dst) =
    emit [ rexW
         , 0xFF                 -- INC
         , modRM + index dst ]
inc _ = nodef

dec :: Value -> X86 ()
dec (Reg dst) =
    emit [ rexW
         , 0xFF                 -- DEC
         , modRM + (index dst + 8) ]
dec _ = nodef

loop :: Value -> X86 ()
loop (Addr dst) = do
    src <- use memoff
    emit [ 0xE2
         , fromIntegral (dst - src) ]
loop _ = nodef

syscall :: X86 ()
syscall =
    emit [ 0x0f
         , 0x05 ]

label :: X86 Value
label = Addr <$> use memoff

asciz :: [Char] -> IO Word32
asciz str = heapPtr <$> newCString str

-- | A simple and easy way to get the index of a register for the ModR/M representation in an instruction.
index :: Reg -> Word8
index = fromIntegral . fromEnum

-- | When there is no definition for an instruction, simply throw an error.
nodef :: X86 a
nodef = throwError "Invalid operation."