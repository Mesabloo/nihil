module Nihil.TypeChecking.Errors.Redefined where

import Nihil.Utils.Source
import Text.PrettyPrint.ANSI.Leijen
import Prelude hiding ((<$>))
import Control.Arrow ((&&&))

{-| [Redefined function error]

    Occurs when a function with the same name already exists in the scope.

    For example:

    > f = 0
    > f = 1

    @f@ already exists when the typechecking of @f = 1@ occurs, it therefore throws this error.
-}
redefinedFunction :: Located String -> Doc
redefinedFunction name = nest 4 (text "- Redefined function" <$> description)
  where description =
            let (ann, loc) = (annotated &&& location) name
            in text "> For function named: " <> text ann <$>
               text "> At: " <> text (show loc) <> line

{-| [Redefined type error]

    Occurs when a type with the same name is already in scope.

    See the documentation for 'redefinedFunction' for more information.
-}
redefinedType :: Located String -> Doc
redefinedType name = nest 4 (text "- Redefined type" <$> description)
  where description =
            let (ann, loc) = (annotated &&& location) name
            in text "> For type named: " <> text ann <$>
               text "> At: " <> text (show loc) <> line