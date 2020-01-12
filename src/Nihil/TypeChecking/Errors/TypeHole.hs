module Nihil.TypeChecking.Errors.TypeHole where

import Nihil.TypeChecking.Core
import Nihil.Utils.Source (location)
import Nihil.TypeChecking.Pretty()
import Text.PrettyPrint.ANSI.Leijen
import Prelude hiding ((<$>))

{-| [Type hole]

    A type hole is a placeholder in expressions. It is used to get the type of an expression which should be placed
    when the type hole is.

    For example:

    > f :: Integer
    > f = _

    This type hole will be unified to @Integer@ and will later display that an expression of type @Integer@ can be inserted there.
-}
typeHole :: Type -> Doc
typeHole t = nest 4 (text "- Found type hole:" <$> description)
  where description :: Doc
        description =
            let loc = location t
            in text "> Fitting type: " <> pretty t <$>
               text "> At: " <> text (show loc) <> line