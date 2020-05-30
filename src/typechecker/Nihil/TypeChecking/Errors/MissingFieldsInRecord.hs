module Nihil.TypeChecking.Errors.MissingFieldsInRecord where

import Nihil.Utils.Source
import Text.PrettyPrint.ANSI.Leijen
import Prelude hiding ((<$>))

missingFieldsInRecord :: [String] -> SourcePos -> Doc
missingFieldsInRecord fs pos =
    nest 4 (text "> Record is missing fields:" <$> description)
  where description =
            text "- Missing fields:" <+> semiBraces (fmap text fs) <$>
            text "- At: " <> text (show pos) <> line
