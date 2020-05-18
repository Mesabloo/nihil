module Nihil.TypeChecking.Errors.Unification where

import Nihil.TypeChecking.Core
import Nihil.Utils.Source (location)
import Nihil.TypeChecking.Pretty()
import Text.PrettyPrint.ANSI.Leijen
import Prelude hiding ((<$>))

{-| [Type unification error]

    Occurs when two types cannot be unified. (i.e. they are incompatible)

    See "Nihil.TypeChecking.Unification.Type" for unification rules.
-}
unifyType :: Type -> Type -> Doc
unifyType t1 t2 = nest 4 (text "- Type mismatch:" <$> description)
  where description :: Doc
        description =
            let loc = location t2
            in text "> Expected type: " <> pretty t1 <$>
               text "> Actual type:   " <> pretty t2 <$>
               text "> At: " <> text (show loc) <> line

{-| [Kind unification error]

    Occurs when two kinds cannot be unified. (i.e. they are incompatible)

    See "Nihil.TypeChecking.Unification.Kind" for unification rules.
-}
unifyKind :: Kind -> Kind -> Doc
unifyKind k1 k2 = nest 4 (text "- Kind mismatch:" <$> description)
  where description :: Doc
        description =
            text "> Expected kind: " <> pretty k1 <$>
            text "> Actual kind:   " <> pretty k2 <> line