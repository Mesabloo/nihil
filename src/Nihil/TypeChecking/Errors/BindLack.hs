module Nihil.TypeChecking.Errors.BindLack where

import Nihil.Utils.Source
import Text.PrettyPrint.ANSI.Leijen
import Prelude hiding ((<$>))
import Control.Arrow ((&&&))

{-| [Bind lack error]

    Occurs when a function has a type declaration but no definition.
    Therefore it makes no sense to let such functions pass, as they won't be evaluable at runtime.

    For example:

    > f :: Integer
    > g :: Char
    > g = 'c'

    As there is no definition for @f@, the typechecker will spit out a bind lack error.

    The only known way to fix this compilation warning is to add a definition to functions lacking binds.
-}
lacksBind :: Located String -> Doc
lacksBind name = nest 4 (text "- Function declaration lacks bind" <$> description)
  where description =
            let (loc, ann) = (location &&& annotated) name
            in text "> In declaration of: " <> text ann <$>
               text "> At: " <> text (show loc) <> line