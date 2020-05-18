{-# LANGUAGE RankNTypes #-}

module Nihil.Runtime.Errors.Developer where

import Nihil.Utils.Impossible
import Text.PrettyPrint.ANSI.Leijen
import Prelude hiding ((<$>))

{-| [Developer error]

    Occurs when the interpreter has an unexpected behavior caused by malfunctioning parts of the compiler.

    This error should be quite rare. If you got to view one, please don't hesitate to report it
    <https://github.com/mesabloo/nihil/issues here> (if there isn't already an issue for that).
-}
developerError :: forall a. String -> a
developerError msg = impossible (show description)
  where description =
            text "─── DEVELOPER ERROR! ───" <$>
            text "Please file a report to <https://github.com/mesabloo/nihil/issues> with as many information as you might have." <$> line <>
            text "Typechecking failed: " <> text msg <> line