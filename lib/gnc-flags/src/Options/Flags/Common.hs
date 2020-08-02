module Options.Flags.Common
( module Options.Flags.Common.Warnings
, pCommonFlags
) where

import Data.Bits
import Options.Flags.Common.Warnings
import Options.Flags.Flag
import Options.Applicative

pCommonFlags :: Parser Flag
pCommonFlags = foldl (.|.) nullFlag <$> sequenceA allParsers
  where
    allParsers = [ pFlag_Wall ]
