module Nihil.TypeChecking.Errors.UnboundName where

import Nihil.Utils.Source
import Text.PrettyPrint.ANSI.Leijen
import Prelude hiding ((<$>))
import Control.Arrow ((&&&))

{-| [Unbound name error]

    Occurs when an encountered symbol doesn't exist in the scope.

    For example:

    > f x = y

    As @y@ doesn't exist, this error will be thrown at your face on your terminal.

    It's however easy to fix, as you just have to define the missing symbol.
-}
unboundName :: Located String -> Doc
unboundName name = nest 4 (text "- Unknown symbol" <+> dquotes (text (annotated name)) <$> description)
  where description =
            let (loc, _) = (location &&& annotated) name
            in text "> At: " <> text (show loc) <> line