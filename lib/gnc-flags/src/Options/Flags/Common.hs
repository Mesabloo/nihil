module Options.Flags.Common
( module Options.Flags.Common.Warnings
, pCommonFlags
) where

import Data.Bits
import Options.Flags.Common.Warnings
import Options.Applicative

pCommonFlags :: Parser Int
pCommonFlags = foldl (.|.) 0 <$> sequenceA allParsers
  where
    allParsers = [ pFlag_Wall ]
