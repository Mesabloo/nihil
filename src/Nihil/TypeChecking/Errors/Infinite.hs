module Nihil.TypeChecking.Errors.Infinite where

import Nihil.TypeChecking.Core
import Nihil.Utils.Source
import Nihil.TypeChecking.Pretty()
import Text.PrettyPrint.ANSI.Leijen
import Prelude hiding ((<$>))

{-| [Infinite kind error]

    Occurs when you attempt to substitute a kind variable with a kind which includes that kind variable.

    For example:

    Trying to substitute @k@ with @Fun k@ fails because @Fun k@ would then expand to @Fun (Fun k)@ which would
    then expand to @Fun (Fun (Fun k))@ etc, thus making an infinite kind.

    It could be possible to handle those with “auto-@'fix'@ing” kinds, where @Fun (Fun (... k))@ would become @'Fix Fun@,
    but it won't be supported in this programming language.
-}
infiniteKind :: String -> Kind -> Doc
infiniteKind v k1 =
    nest 2 (text "- Occur check failed" <$> description)
  where description :: Doc
        description =
            text "> Cannot construct an infinite kind" <$>
            text "> In kind: " <> pretty k1 <> line

{-| [Infinite type error]

    Occurs when a substitution attempt ends in the same type variable being on both sides of the substitution.

    See 'infiniteKind' for more documentation.
-}
infiniteType :: String -> Type -> Doc
infiniteType v t1 =
    nest 2 (text "- Occur check failed" <$> description)
  where description :: Doc
        description =
            let pt1 = pretty t1
                loc = location t1
            in text "> Cannot construct an infinite type" <$>
               text "> In type: " <> pt1 <$>
               text "> Constraint failed: " <> text v <> text " ~ " <> pt1 <$>
               text "> At: " <> text (show loc) <> line