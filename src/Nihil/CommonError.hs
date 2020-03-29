{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiWayIf #-}

module Nihil.CommonError where

import Nihil.Utils.Source
import Data.Void (Void)
import qualified Text.Megaparsec as MP
import Text.PrettyPrint.ANSI.Leijen hiding (Pretty, pretty, space)
import qualified Data.Text as T
import Prelude hiding ((<$>))
import Data.List.NonEmpty (NonEmpty((:|)))

data CompilerError
    = CError { pos :: SourcePos, err :: AnyError }
  deriving (Show, Eq)

data AnyError
    = ParseError (MP.ParseErrorBundle T.Text Void)
  deriving (Show, Eq)

class Pretty s a where
    pretty :: [s] -> a -> Doc

instance Pretty T.Text CompilerError where
    pretty input CError{..} = case pos of
        NoSource             -> undefined
        Loc line column file -> let spacing c = space (length (show line)) c in
            empty <+> blue (spacing '─') <> blue (text "─┬─") <+> white (bold (pretty input pos)) <+> blue (text "──") <> hardline <>
            empty <+> spacing ' ' <+> blue (text "│") <> hardline <>
            showMultipleLinesIfNeeded line input <> showOneLine line id input <> hardline <>
--dullblue (text (show line)) <+> blue (text "│") <+> empty <+> white (text (T.unpack (input !! (unPos line - 1)))) <> hardline <>
            empty <+> spacing ' ' <+> blue (text "│") <+> empty <+> space (unPos column - 1) ' ' <> red (text "^" <+> prettyError err) <> hardline <>
            empty <+> spacing ' ' <+> blue (text "│") <> hardline <>
            empty <+> spacing ' ' <+> blue (text "└─╸") <+> dullred (bold (text "Fatal compiler error detected")) <> hardline

showMultipleLinesIfNeeded :: Position -> [T.Text] -> Doc
showMultipleLinesIfNeeded line input
    | unPos line - 2 >= 1 =
        showOneLine line (2 `subtract`) input <> hardline <>
        showOneLine line (1 `subtract`) input <> hardline
    | unPos line - 1 >= 1 =
        showOneLine line (1 `subtract`) input <> hardline
    | otherwise           = empty

showOneLine :: Position -> (Int -> Int) -> [T.Text] -> Doc
showOneLine line transform input =
    empty <+> dullblue (int (transform (unPos line))) <+> blue (text "│") <+> empty <+> white (text (T.unpack (input !! (transform (unPos line) - 1))))

space :: Int -> Char -> Doc
space n sep = text (replicate n sep)

prettyError :: AnyError -> Doc
prettyError (ParseError MP.ParseErrorBundle{..}) =
    let parseError :| _ = bundleErrors
    in text (head (lines (MP.parseErrorTextPretty parseError)))

instance Pretty T.Text SourcePos where
    pretty _ NoSource            = text "<unknown>:0:0"
    pretty i (Loc line col file) = angles (text file) <> colon <> pretty i line <> colon <> pretty i col

instance Pretty T.Text Position where
    pretty _ p = int (unPos p)
