{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DeriveAnyClass #-}

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
type Labels = Set.Set Label
type Batches = Set.Set Batch

data Batch
    = Batch Labels
  deriving (Eq)

instance Monoid Batch where
    mempty = Batch mempty

instance Semigroup Batch where
    Batch b1 <> Batch b2 = Batch (b1 <> b2)

data Severity
    = Error
    | Warning

data Diagnostic
    = Diag Severity FileContent Message Batches [Note]

instance Monoid Diagnostic where
    mempty = newDiagnostic Error

instance Semigroup Diagnostic where
    Diag Error file1 msg1 labels1 notes1 <> Diag Error _ msg2 labels2 notes2 = Diag Error file1 (msg1 <> "\n" <> msg2) (labels1 <> labels2) (notes1 <> notes2)
    Diag Warning file1 msg1 labels1 notes1 <> Diag Warning _ msg2 labels2 notes2 = Diag Warning file1 (msg1 <> "\n" <> msg2) (labels1 <> labels2) (notes1 <> notes2)
    d@(Diag Error _ _ _ _) <> _ = d
    _ <> d@(Diag Error _ _ _ _) = d

data Style
    = Primary
    | Secondary
  deriving Eq

data Label
    = Label Style SourcePos Message
    | Ellipsis SourcePos
  deriving Eq

instance Ord Batch where
    Batch b1 <= Batch b2
        | null b1 && null b2 = True
        | null b1            = True
        | null b2            = False
        | otherwise          = Set.elemAt 0 b1 <= Set.elemAt 0 b2

instance Ord Label where
    Label _ pos1 _ <= Label _ pos2 _ = pos1 <= pos2
    Label _ pos1 _ <= Ellipsis pos2  = pos1 <= pos2
    Ellipsis pos1  <= Label _ pos2 _ = pos1 <= pos2
    Ellipsis pos1  <= Ellipsis pos2  = pos1 <= pos2

primaryLabel, secondaryLabel :: SourcePos -> Label
primaryLabel pos   = Label Primary pos ""
secondaryLabel pos = Label Secondary pos ""

withLabelMessage :: Label -> Message -> Label
withLabelMessage (Label style pos _) msg = Label style pos msg


newBatch :: Batch
newBatch = Batch mempty

withLabels :: Batch -> [Label] -> Batch
withLabels (Batch _) labels = Batch (Set.fromList labels)

andAddLabel :: Batch -> Label -> Batch
andAddLabel (Batch labels) label = Batch (label `Set.insert` labels)


newDiagnostic :: Severity -> Diagnostic
newDiagnostic sev = Diag sev mempty "" mempty mempty

errorDiagnostic, warningDiagnostic :: Diagnostic
errorDiagnostic = newDiagnostic Error
warningDiagnostic = newDiagnostic Warning

withCode :: Diagnostic -> FileContent -> Diagnostic
withCode (Diag sev _ msg labels notes) content = Diag sev content msg labels notes

withMessage :: Diagnostic -> Message  -> Diagnostic
withMessage (Diag sev content _ labels notes) msg = Diag sev content msg labels notes

withBatches :: Diagnostic -> [Batch] -> Diagnostic
withBatches (Diag sev content msg _ notes) labels = Diag sev content msg (Set.fromList labels) notes

withNotes :: Diagnostic -> [Note] -> Diagnostic
withNotes (Diag sev content msg labels _) notes = Diag sev content msg labels notes

andAddBatch :: Diagnostic -> Batch -> Diagnostic
andAddBatch (Diag sev content msg labels notes) label = Diag sev content msg (label `Set.insert` labels) notes

andAddNote :: Diagnostic -> Note -> Diagnostic
andAddNote (Diag sev content msg labels notes) note = Diag sev content msg labels (note : notes)



type CompilerError = Diagnostic

instance Pretty Diagnostic where
    pretty (Diag sev content msg batches notes) =
        let severity = case sev of
                Error   -> bold (red (text "[error]"))
                Warning -> bold (yellow (text "[warning]"))
        in severity <+> bold (white (text msg)) <> hardline <>
           showAllBatches content batches <>

           showAllNotes notes

showAllBatches :: FileContent -> Set.Set Batch -> Doc
showAllBatches content batches
    | null batches = empty
    | otherwise    =
        let (Batch labels, remaining) = Set.deleteFindMin batches
        in showAllLabels content labels <>
           showAllBatches content remaining

showAllLabels :: FileContent -> Set.Set Label -> Doc
showAllLabels content labels
    | null labels = empty
    | otherwise   =
        let Label _ pos _ = Set.elemAt 0 labels
            maxLine       = case Set.lookupMax labels of
                Nothing            -> pos ^. sourceLine
                Just (Label _ p _) -> p ^. sourceLine
                Just (Ellipsis p)  -> p ^. sourceLine
            !padding      = text . replicate (length (show maxLine))

            lbls = if length labels <= 3
                   then showLabels content padding labels
                   else
                       let labels'               = Set.elems labels
                           (end, l3:_)           = span (\(Label sev _ _) -> sev /= Primary) (reverse labels')
                           l1:l2:(Label _ p _):_ = labels'
                       in showLabels content padding (Set.fromList ([l1, l2, Ellipsis p, l3] <> reverse end))
        in
            empty <+> cyan (padding ' ')   <+> cyan (text "├─────") <+> bold (white (text (pos ^. sourceFile))) <+> cyan (text "────") <> hardline <>
            empty <+> cyan (padding ' ')   <+> cyan (text "│")                                                                         <> hardline <>
            lbls                                                                                                                                   <>
            empty <+> cyan (padding ' ')   <+> cyan (text "│")                                                                         <> hardline

showLabels :: FileContent -> (Char -> Doc) -> Set.Set Label -> Doc
showLabels content padding labels
    | null labels = empty
    | otherwise   =
        let label                = Set.elemAt 0 labels
            pos                  = case label of
                Label _ p _ -> p
                Ellipsis p  -> p
            lineNumber           = unPos (pos ^. sourceLine)

            divide (Label _ p _) = p ^. sourceLine == Pos lineNumber
            divide (Ellipsis p)  = p ^. sourceLine == Pos lineNumber
            (toShow, rem)        = Set.spanAntitone divide labels

            lineToShow           = T.unpack (content !! (lineNumber - 1))
                                         --          ^^ Very unsafe here but we shouldn't be able to under/overflow
            item                 =
                if | Label{} <- label -> empty <+> blue (int lineNumber) <+> cyan (text "│  ")    <+> white (text lineToShow) <> hardline
                   | otherwise        -> empty

        in item                                                                                                                                    <>
           foldl (prettify padding) empty toShow                                                                                                   <>

           showLabels content padding rem
  where prettify :: (Char -> Doc) -> Doc -> Label -> Doc
        prettify padding doc label = doc                                                                                                           <>
           empty <+> cyan (padding ' ')    <+> cyan (text "│  ")    <+> pretty label                                                   <> hardline

instance Pretty Label where
    pretty (Label style pos message) =
        let columnNumber = unPos (pos ^. sourceColumn)

            styled t = case style of
                Primary   -> dullred (text "^"  <+> t)
                Secondary -> dullblue (text "-" <+> t)

            rpadding = text (replicate (columnNumber - 1) ' ')
        in rpadding <> styled (text message)
    pretty (Ellipsis _) = text "  ⋮"

instance Pretty SourcePos where
    pretty NoSource          = text "<unknown>:0"
    pretty (Loc line _ file) = angles (text file) <> colon <> int (unPos line)

showAllNotes :: [Note] -> Doc
showAllNotes = foldl prettyNote empty
  where prettyNote doc n = doc <> pretty n <> hardline

instance {-# OVERLAPPING #-} Pretty Note where
    pretty n = empty <+> cyan equals <+> white (text n)
