module Nihil.TypeChecking.Errors.GADTWrongReturnType where

import Nihil.TypeChecking.Core
import Nihil.Utils.Source
import Nihil.TypeChecking.Pretty()
import Text.PrettyPrint.ANSI.Leijen
import Prelude hiding ((<$>))

{-| [GADT constructor - wrong return type]

    Occurs when a GADT constructor has a constructor whose return type cannot be unified with the base type
    of the GADT.

    For example:

    > data GADT a where
    >     Case1 :: Integer → GADT
    >     Case2 :: Double → List Double

    This would yield two errors:

    * @Case1@ returns a type which does not kindcheck,
      because @GADT@ has kind @* → *@ and @(→)@ has kind @* → * → *@ and both cannot be unified
      (we write it @* → * ≁ *@).

    * @Case2@ returns a type which is not unifiable with the @GADT@ data type.

    If you really had this error once, you should consider learning about GADTs.
-}
gadtWrongReturnType :: String -> Type -> Type -> Doc
gadtWrongReturnType name actual expected =
    nest 4 (text "- Cannot unify GADT constructor return type" <$> description)
  where description =
            let loc = location actual
            in text "> For the data constructor: " <> text name <$>
               text "> Expected type: " <> pretty expected <$>
               text "> Actual type:   " <> pretty actual <$>
               text "> At: " <> text (show loc) <> line