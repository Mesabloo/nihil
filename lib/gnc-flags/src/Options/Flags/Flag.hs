{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Options.Flags.Flag
( Flag, FlagSet, nullFlag, flag ) where

import Data.Bits (Bits)

newtype Flag = Flag Integer
  deriving (Show, Eq, Ord, Bits, Enum, Num, Real, Integral)

nullFlag :: Flag
nullFlag = Flag 0

flag :: Integer -> Flag
flag = Flag

type FlagSet = Flag
