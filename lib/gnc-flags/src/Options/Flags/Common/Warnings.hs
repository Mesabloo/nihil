{-# LANGUAGE BinaryLiterals #-}

module Options.Flags.Common.Warnings where

import Data.Bits
import Data.Bool
import Options.Flags.Flag
import Options.Applicative hiding (flag)
import Data.Functor ((<&>))

flag_Wall :: Flag
flag_Wall =
  nullFlag
{-# INLINE flag_Wall #-}

     -- We do not have any flag yet, so we keep this as 0. Later on, we will just
     -- concatenate all the warning flags with (.|.).

pFlag_Wall :: Parser Flag
pFlag_Wall =
  optional (strOption (long "warn" <> short 'W' <> help "Turn on all non-agressive warnings.")) <&> \case
    Just "all" -> flag_Wall
    _          -> nullFlag
