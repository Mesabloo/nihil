module Nihil.TypeChecking.Errors.UndefinedType where

import Text.PrettyPrint.ANSI.Leijen
import Prelude hiding ((<$>))

{-| [Undefined kind error]

    Occurs when a type is not found in the current scope.

    For example:

    > type T = Maybe w

    THe type variable @w@ doesn't exist in the scope, and so has an undefined kind.
-}
undefinedType :: String -> Doc
undefinedType n = nest 4 (text "- Kind not found" <$> description)
  where description :: Doc
        description =
            text "> For type named: " <> text n <> line