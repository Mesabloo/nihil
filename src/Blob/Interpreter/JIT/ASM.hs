{-# LANGUAGE TemplateHaskell, CPP #-}

-- | This module holds all the assembly DSL instructions.
module Blob.Interpreter.JIT.ASM where

import Blob.Interpreter.JIT.TH
import Blob.Interpreter.JIT.Types
import Blob.Interpreter.JIT.Compilation
import Data.Bits
import Control.Monad.Except
import Data.Word
import Control.Lens hiding (index)

#define REX_W 0x48
#define MOD_111 0xC0

-- https://www.felixcloutier.com/x86/

$(mkRegFuns)

ret :: X86 ()
ret = do
    emit [0xc3]

add :: Value -> Value -> X86 ()
add (Reg l) (Int r) = do
    emit [REX_W]
    emit [0x05]              -- AddrDD
    imm r
add (Reg l) (Reg r) = do
    emit [REX_W]
    emit [0x01]              -- AddrDD
    emit [MOD_111 .|. index r `shiftL` 3 .|. index l]
add _ _ = nodef

sub :: Value -> Value -> X86 ()
sub (Reg l) (Int r) = do
    emit [REX_W]
    emit [0x2D]              -- SUB
    imm r
sub (Reg l) (Reg r) = do
    emit [REX_W]
    emit [0x29]              -- SUB
    emit [MOD_111 .|. index r `shiftL` 3 .|. index l]
sub _ _ = nodef

push :: Value -> X86 ()
push (Reg l) = do
    emit [0x50 + index l]
push _ = nodef

pop :: Value -> X86 ()
pop (Reg l) = do
    emit [0x58 + index l]
pop _ = nodef

call :: Value -> X86 ()
call (Addr dst) = do
    emit [0xE8]
    src <- use memoff
    imm (dst - (src + 5))
call _ = nodef

mul :: Value -> X86 ()
mul (Reg l) = do
    emit [REX_W]
    emit [0xF7]
    emit [0xE0 .|. index l]
mul _ = nodef

imul :: Value -> Value -> X86 ()
imul (Reg l) (Int r) = do
    emit [REX_W]
    emit [0x69]
    emit [MOD_111 .|. index l]
    imm r
imul (Reg l) (Reg r) = do
    emit [REX_W]
    emit [0x0F]
    emit [0xAF]
    emit [0xC0 .|. index r `shiftL` 3 .|. index l]
imul _ _ = nodef

mov :: Value -> Value -> X86 ()
mov (Reg dst) (Int src) = do
    emit [REX_W]
    emit [0xC7]
    emit [0xC0 .|. (index dst .&. 7)]
    imm src
mov (Reg dst) (Addr src) = do
    emit [REX_W]
    emit [0xC7]
    emit [0xC7]
    imm src
mov (Reg dst) (Reg src) = do
    emit [REX_W]
    emit [0x89]              -- MOV
    emit [0xC0 .|. index src `shiftL` 3 .|. index dst]
mov _ _ = nodef

nop :: X86 ()
nop = do
    emit [0x90]

inc :: Value -> X86 ()
inc (Reg dst) = do
    emit [REX_W]
    emit [0xFF]              -- INC
    emit [MOD_111 + index dst]
inc _ = nodef

dec :: Value -> X86 ()
dec (Reg dst) = do
    emit [REX_W]
    emit [0xFF]              -- DEC
    emit [MOD_111 + (index dst + 8)]
dec _ = nodef

loop :: Value -> X86 ()
loop (Addr dst) = do
    emit [0xE2]
    src <- use memoff
    emit [fromIntegral $ dst - src]
loop _ = nodef

syscall :: X86 ()
syscall = do
    emit [0x0f]
    emit [0x05]

-- | A simple and easy way to get the index of a register for the ModR/M representation in an instruction.
index :: Reg -> Word8
index = fromIntegral . fromEnum

-- | When there is no definition for an instruction, simply throw an error.
nodef :: X86 a
nodef = throwError "Invalid operation."