{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}

module Text.Diagnose.Report
( Diagnostic, Report, Marker(..)
, diagnostic, (<~<), (<++>)
, reportError, reportWarning
, hint

, printDiagnostic, pretty
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
import Data.Maybe (fromJust)

type Files s a = Map FilePath [s a]

data Diagnostic s m a
  = Diagnostic (Files s a) [Report m]

type Markers m = Map Position (NonEmpty (Marker m))

data Report m
  = Report Kind m (Markers m) [Hint m]

data Kind
  = Error
  | Warning

data Hint m
  = Hint m

data Marker m
  = (:^^^^:) m
  | (:----:) m
  | (:~~~~:) m
  | (:++++:)


diagnostic :: Diagnostic s m a
diagnostic = Diagnostic mempty mempty

(<~<) :: Diagnostic s m a -> (FilePath, [s a]) -> Diagnostic s m a
Diagnostic files reports <~< (path, content) = Diagnostic (Map.insert path content files) reports

(<++>) :: Diagnostic s m a -> Report m -> Diagnostic s m a
Diagnostic files reports <++> report = Diagnostic files (reports ++ [report])


infixl 5 <++>
infixr 4 <~<

reportError, reportWarning :: m -> [(Position, Marker m)] -> [Hint m] -> Report m
reportError = newReport Error
reportWarning = newReport Warning

newReport :: Kind -> m -> [(Position, Marker m)] -> [Hint m] -> Report m
newReport sev msg markers hints = Report sev msg markMap hints
  where markMap               = foldl createMap mempty markers
        createMap m (p, mark) = Map.insertWith (flip (<>)) p (mark List.:| []) m


hint :: m -> Hint m
hint = Hint


instance (Foldable s, Pretty (s a), Pretty m) => Pretty (Diagnostic s m a) where
  pretty (Diagnostic files reports) = indent 1 (sep (fmap (prettyReport files) reports)) <> line

prettyReport :: (Foldable s, Pretty (s a), Pretty m) => Files s a -> Report m -> Doc
prettyReport files (Report kind msg markers hints) =
  let (color, margin, sev) = prettyKind kind
  in color (bold sev) <> colon <+> pretty msg <$>
     prettyCodeWithMarkers files markers color margin <$> line <>
     prettyHints hints

prettyKind :: Kind -> (Doc -> Doc, Int, Doc)
prettyKind Error   = (red, 7, brackets $ text "error")
prettyKind Warning = (yellow, 9, brackets $ text "warning")

prettyCodeWithMarkers :: (Foldable s, Pretty m, Pretty (s a)) => Files s a -> Markers m -> (Doc -> Doc) -> Int -> Doc
prettyCodeWithMarkers files markers color margin =
  let sortedMarkers = sortBy (compare `on` fst) (Map.toList markers)
      showFile f = mconcat (replicate (margin - 2) space) <> text "In" <> colon <+> green f
  in case sortedMarkers of
    []                                   -> showFile (text "???")
    (Position{beginning=begin, ..}, _):_ ->
      let (bLine, bCol)  = begin
          ((p, _):_)     = reverse sortedMarkers
          maxLineMarkLen = length (show (fst (beginning p)))

          showLine l     =
            let lineMarkLen = length (show l)
            in space <> text (replicate (maxLineMarkLen - lineMarkLen) ' ') <> integer l <> text "|"

          fileContent    = fromJust (Map.lookup file files)

          showMarkers    = sortedMarkers <&> uncurry \ Position{..} markers ->
            let (bLine, bCol)  = beginning
                (eLine, eCol)  = end

                code           = fileContent !! fromIntegral (bLine - 1)

                underlineLen   = fromIntegral $ (if eLine == bLine then eCol else fromIntegral (length code)) - bCol

                pretty'        = fillSep . fmap text . words . show . pretty
                marker m       = case m of
                  (:^^^^:) msg -> [color $ text (replicate underlineLen '^') <+> align (pretty' msg)]
                  (:----:) msg -> [magenta $ text (replicate underlineLen '-') <+> align (pretty' msg)]
                  (:~~~~:) msg -> [dullgreen $ text (replicate underlineLen '~') <+> align (pretty' msg)]
                  (:++++:)     -> []
                renderMarker m =
                  case marker m of
                    []  -> []
                    x:_ -> [mconcat (replicate (maxLineMarkLen + 2 + fromIntegral bCol) space) <> x]

                renderedMarkers = List.toList markers >>= renderMarker
            in white $ bold (showLine bLine) <+> pretty code <>
               mconcat ((if null renderedMarkers then id else (line :)) $ punctuate line renderedMarkers)
      in showFile (text file) <$>
         empty <$>
         mconcat (punctuate line showMarkers)

prettyHints :: (Pretty m) => [Hint m] -> Doc
prettyHints [] = line
prettyHints hs = blue (fillSep $ punctuate line (fmap render hs)) <> line
  where render (Hint msg) = fillSep (fmap text (words (show (pretty msg))))


printDiagnostic handle diag = displayIO handle (renderPretty 0.9 80 $ pretty diag)
