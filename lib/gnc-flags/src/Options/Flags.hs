module Options.Flags
( module Options.Flags.Common
, module Options.Flags.Parser
, module Options.Flags.Typechecker
, module Options.Flags.Optimisation
, module Options.Flags.Codegen
, pFlags
, isFlagOn
) where

import Options.Flags.Common
import Options.Flags.Parser
import Options.Flags.Typechecker
import Options.Flags.Optimisation
import Options.Flags.Codegen

import Data.Bits
import Options.Applicative

pFlags :: Parser Integer
pFlags = foldl (.|.) 0 <$> sequenceA all
  where
    all = [ pCommonFlags ]

-- | Tests whether a set of flags (all concatenated) contains a given flag.
isFlagOn :: Integer -> Integer -> Bool
isFlagOn flagSet flag = (flagSet .&. flag) `xor` zeroBits /= zeroBits
