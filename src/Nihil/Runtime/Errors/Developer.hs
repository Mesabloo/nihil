{-# LANGUAGE RankNTypes #-}

module Nihil.Runtime.Errors.Developer where

import Nihil.Utils.Impossible
import Text.PrettyPrint.ANSI.Leijen
import Prelude hiding ((<$>))

developerError :: forall a. String -> a
developerError msg = impossible (show description)
  where description =
            text "─── DEVELOPER ERROR! ───" <$>
            text "Please file a report to <https://github.com/mesabloo/nihil/issues> with as many information as you might have." <$> line <>
            text "Typechecking failed: " <> text msg <> line