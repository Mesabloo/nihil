{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Nihil.CommonError where

import Nihil.Utils.Source
import Text.PrettyPrint.ANSI.Leijen
import qualified Data.Text as T
import Prelude hiding ((<$>))
import qualified Data.Set as Set
import Control.Lens ((^.))

type FileContent = [T.Text]
type Message = String
type Note = String

data Severity
    = Error
    | Warning

data Diagnostic
    = Diag Severity FileContent Message (Set.Set Label) [Note]

data Style
    = Primary
    | Secondary
  deriving Eq

data Label
    = Label Style SourcePos Message
  deriving Eq

instance Ord Label where
    Label _ pos1 _ <= Label _ pos2 _ = pos1 <= pos2

primaryLabel, secondaryLabel :: SourcePos -> Label
primaryLabel pos   = Label Primary pos ""
secondaryLabel pos = Label Secondary pos ""

withLabelMessage :: Label -> Message -> Label
withLabelMessage (Label style pos _) msg = Label style pos msg


newDiagnostic :: Severity -> Diagnostic
newDiagnostic sev = Diag sev mempty "" mempty mempty

errorDiagnostic, warningDiagnostic :: Diagnostic
errorDiagnostic = newDiagnostic Error
warningDiagnostic = newDiagnostic Warning

withCode :: Diagnostic -> FileContent -> Diagnostic
withCode (Diag sev _ msg labels notes) content = Diag sev content msg labels notes

withMessage :: Diagnostic -> Message  -> Diagnostic
withMessage (Diag sev content _ labels notes) msg = Diag sev content msg labels notes

withLabels :: Diagnostic -> [Label] -> Diagnostic
withLabels (Diag sev content msg _ notes) labels = Diag sev content msg (Set.fromList labels) notes

withNotes :: Diagnostic -> [Note] -> Diagnostic
withNotes (Diag sev content msg labels _) notes = Diag sev content msg labels notes

andAddLabel :: Diagnostic -> Label -> Diagnostic
andAddLabel (Diag sev content msg labels notes) label = Diag sev content msg (label `Set.insert` labels) notes

andAddNote :: Diagnostic -> Note -> Diagnostic
andAddNote (Diag sev content msg labels notes) note = Diag sev content msg labels (note : notes)



type CompilerError = Diagnostic

instance Pretty Diagnostic where
    pretty (Diag sev content msg labels notes) =
        let severity = case sev of
                Error   -> bold (red (text "[error]"))
                Warning -> bold (yellow (text "[warning]"))
        in severity <+> bold (white (text msg)) <> hardline <>
           showAllLabels content labels <>
           showAllNotes notes

showAllLabels :: FileContent -> Set.Set Label -> Doc
showAllLabels content labels
    | null labels = empty
    | otherwise   =
        let Label _ pos _ = Set.elemAt 0 labels
            lineNumber    = unPos (pos ^. sourceLine)

            (toShow, rem) = Set.spanAntitone (\(Label _ p _) -> p ^. sourceLine == Pos lineNumber) labels

            lineToShow    = T.unpack (content !! (lineNumber - 1))
                                  --          ^^ Very unsafe here but we shouldn't be able to under/overflow

            lastElem      = Set.lookupMax rem
            maxLine       = case lastElem of
                Nothing            -> pos ^. sourceLine
                Just (Label _ p _) -> p ^. sourceLine

            !padding      = text . replicate (length (show maxLine))

        in
           empty <+> cyan (padding ' ')    <+> cyan (text "├────") <+> bold (white (pretty pos)) <+> cyan (text "──") <> hardline <>
           empty <+> cyan (padding ' ')    <+> cyan (text "│")                                                        <> hardline <>
           empty <+> blue (int lineNumber) <+> cyan (text "│  ")   <+> white (text lineToShow)                        <> hardline <>

           foldl (prettify padding) empty toShow                                                                                  <>
           empty <+> cyan (padding ' ')    <+> cyan (text "│")                                                        <> hardline <>

           showAllLabels content rem
  where prettify :: (Char -> Doc) -> Doc -> Label -> Doc
        prettify padding doc label = doc                                                                                          <>
           empty <+> cyan (padding ' ')    <+> cyan (text "│  ")   <+> pretty label                                   <> hardline

instance Pretty Label where
    pretty (Label style pos message) =
        let columnNumber = unPos (pos ^. sourceColumn)

            styled t = case style of
                Primary   -> dullred (text "^"  <+> t)
                Secondary -> dullblue (text "-" <+> t)

            rpadding = text (replicate (columnNumber - 1) ' ')
        in rpadding <> styled (text message)

instance Pretty SourcePos where
    pretty NoSource          = text "<unknown>:0"
    pretty (Loc line _ file) = angles (text file) <> colon <> int (unPos line)

showAllNotes :: [Note] -> Doc
showAllNotes = foldl prettyNote empty
  where prettyNote doc n = doc <> pretty n <> hardline

instance {-# OVERLAPPING #-} Pretty Note where
    pretty n = empty <+> cyan equals <+> white (text n)
