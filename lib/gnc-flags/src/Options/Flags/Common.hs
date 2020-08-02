module Options.Flags.Common
( module Options.Flags.Common.Warnings
, pCommonFlags
) where

import Data.Bits
import Options.Flags.Common.Warnings
import Options.Applicative

pCommonFlags :: Parser Integer
pCommonFlags = foldl setNthBitIfNot0 0 <$> sequenceA allParsers
  where
    setNthBitIfNot0 seq 0 = seq
    setNthBitIfNot0 seq n = setBit seq n


    allParsers = [ pFlag_Wall ]
