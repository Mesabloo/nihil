{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}

module Text.Diagnose.Report
( Report, Marker(..), Files
, reportError, reportWarning
, hint

, prettyReport
) where

import Text.Diagnose.Position
import Text.PrettyPrint.ANSI.Leijen
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as List
import Data.String
import Prelude hiding ((<$>))
import Data.Functor ((<&>))
import Data.Bifunctor (second)
import Data.Function (on)
import Data.List (sortBy)
import Data.Maybe (fromJust, maybeToList)

type Files s a = Map FilePath [s a]
type Markers m = Map Position (NonEmpty (Marker m))

data Report m
  = Report Kind m (Markers m) [Hint m]

data Kind
  = Error
  | Warning

data Hint m
  = Hint m

data Marker m
  = This m
  | Where m
  | Maybe m
  | Empty


reportError, reportWarning :: m -> [(Position, Marker m)] -> [Hint m] -> Report m
reportError = newReport Error
reportWarning = newReport Warning

newReport :: Kind -> m -> [(Position, Marker m)] -> [Hint m] -> Report m
newReport sev msg markers hints = Report sev msg markMap hints
  where markMap               = foldl createMap mempty markers
        createMap m (p, mark) = Map.insertWith (flip (<>)) p (mark List.:| []) m


hint :: m -> Hint m
hint = Hint



prettyReport :: (Foldable s, Pretty (s a), Pretty m) => Files s a -> Report m -> Doc
prettyReport files (Report kind msg markers hints) =
  let (color, margin, sev) = prettyKind kind
  in color (bold sev) <> colon <+> pretty msg <$>
     mconcat (replicate (margin - 2) space) <> text "In" <> colon <+>
     prettyCodeWithMarkers files markers color <$> line <>
     prettyHints hints

prettyKind :: Kind -> (Doc -> Doc, Int, Doc)
prettyKind Error   = (red, 7, brackets $ text "error")
prettyKind Warning = (yellow, 9, brackets $ text "warning")

prettyCodeWithMarkers :: (Foldable s, Pretty m, Pretty (s a)) => Files s a -> Markers m -> (Doc -> Doc) -> Doc
prettyCodeWithMarkers files markers color =
  let sortedMarkers = sortBy (compare `on` fst) (Map.toList markers)
  in case sortedMarkers of
    []                                   -> green (text "???")
    (Position{beginning=begin, ..}, _):_ ->
      let (bLine, bCol)  = begin
          ((p, _):_)     = reverse sortedMarkers
          maxLineMarkLen = length (show (fst (beginning p)))

          showLine l     =
            space <> text (replicate (maxLineMarkLen - length (show l)) ' ') <> integer l <> text "|"

          fileContent    = fromJust (Map.lookup file files)

          showMarkers    = sortedMarkers <&> uncurry \ Position{..} markers ->
            let (bLine, bCol)  = beginning
                (eLine, eCol)  = end

                code           = fileContent !! fromIntegral (bLine - 1)

                underlineLen   = fromIntegral $ (if eLine == bLine then eCol else fromIntegral (length code)) - bCol

                marker m       = prettyMarker underlineLen m color magenta dullgreen
                renderMarker m =
                  marker m <&> \ x -> mconcat (replicate (maxLineMarkLen + 2 + fromIntegral bCol) space) <> x

                renderedMarkers = List.toList markers >>= maybeToList . renderMarker
            in white $ bold (showLine bLine) <+> pretty code <>
               mconcat (applyIfNotNull (line :) $ punctuate line renderedMarkers)
      in green (text file) <$>
         empty <$>
         mconcat (punctuate line showMarkers)

prettyHints :: (Pretty m) => [Hint m] -> Doc
prettyHints [] = line
prettyHints hs = blue (fillSep $ punctuate line (fmap render hs)) <> line
  where render (Hint msg) = smartPretty msg

prettyMarker :: (Pretty m) => Int -> Marker m -> (Doc -> Doc) -> (Doc -> Doc) -> (Doc -> Doc) -> Maybe Doc
prettyMarker underlineLen marker colorThis colorWhere colorMaybe = case marker of
  Empty     -> Nothing
  This msg  -> Just (colorThis $ under '^' <+> align (smartPretty msg))
  Where msg -> Just (colorWhere $ under '-' <+> align (smartPretty msg))
  Maybe msg -> Just (colorMaybe $ under '~' <+> align (smartPretty msg))
 where under   = text . replicate underlineLen

smartPretty :: (Pretty d) => d -> Doc
smartPretty = fillSep . fmap text . words . show . pretty

applyIfNotNull :: ([a] -> [a]) -> [a] -> [a]
applyIfNotNull _ [] = []
applyIfNotNull f l = f l
