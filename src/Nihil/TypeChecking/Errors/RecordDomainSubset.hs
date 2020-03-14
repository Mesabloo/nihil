{-# LANGUAGE FlexibleInstances #-}

module Nihil.TypeChecking.Errors.RecordDomainSubset where

import qualified Data.Set as Set
import Nihil.Utils.Source
import Text.PrettyPrint.ANSI.Leijen
import Prelude hiding ((<$>))

cannotSubtypeRecordDomains :: Set.Set String -> Set.Set String -> SourcePos -> Doc
cannotSubtypeRecordDomains expected got pos =
    nest 4 (text "- Record domains do not subset" <$> description)
  where description =
          text "> Expected at least: " <> pretty expected <$>
          text "> Got:               " <> pretty got <$>
          text "> At: " <> text (show pos) <> line

instance Pretty (Set.Set String) where
    pretty set = semiBraces (fmap text (Set.toList set))
