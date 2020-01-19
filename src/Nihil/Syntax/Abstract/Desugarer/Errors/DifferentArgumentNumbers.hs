module Nihil.Syntax.Abstract.Desugarer.Errors.DifferentArgumentNumbers where

import Nihil.Syntax.Pretty()
import Nihil.Utils.Source
import Text.PrettyPrint.ANSI.Leijen
import Prelude hiding ((<$>))

differentNumberOfArguments :: Located String -> Int -> Int -> Doc
differentNumberOfArguments name expected actual =
    nest 4 (text "- Equations for" <+> dquotes (text (annotated name)) <+> text "have different numbers of arguments" <$> description)
  where description =
            let loc = location name
            in text "> Expected: " <> int expected <$>
               text "> Got:      " <> int actual <$>
               text "> At: " <> text (show loc)