module Nihil.TypeChecking.Errors.NonVisibleMemberOfClass where

import Text.PrettyPrint.ANSI.Leijen
import Prelude hiding ((<$>))
import Nihil.Utils.Source

nonVisibleMemberOf :: String -> String -> SourcePos -> Doc
nonVisibleMemberOf cls fun pos =
    nest 4 (text "- Non visible method of class" <$> description)
  where description =
            text "> For the symbol: " <> text fun <$>
            text "> In the class: " <> text cls <$>
            text "> At: " <> text (show pos) <> line