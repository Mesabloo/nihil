{-# LANGUAGE RecordWildCards #-}

module Text.Diagnose.Position where

import Text.PrettyPrint.ANSI.Leijen

data Position
  = Position
  { beginning :: (Integer, Integer)
  , end       :: (Integer, Integer)
  , file      :: String             }
  deriving (Show, Eq)

instance Pretty Position where
  pretty Position{..} =
    let (bLine, bCol) = beginning
    in angles (text file <> colon <> integer bLine <> colon <> integer bCol)

instance Ord Position where
  p1 <= p2 =
    let (b1Line, b1Col) = beginning p1
        (b2Line, b2Col) = beginning p2
    in b1Line <= b2Line && b1Col <= b2Col
