{-# LANGUAGE RankNTypes #-}

{-| Helper functions for describing unexpected/impossible behaviors
    of the compiler.
-}

module Nihil.Utils.Impossible
( impossible ) where

import GHC.Stack (callStack, prettyCallStack, HasCallStack)

-- | Stops the running program and displays a message with @"The impossible happened!"@.
--
--   Also displays the call stack to see where the error occurred. It is useful to join it
--   in the report of the error, along with details and what triggered it.
impossible :: forall a. HasCallStack => String -> a
impossible msg =
    let abstract =
            "GNC: The impossible happened!\n"
            <> "Message: " <> msg <> "\n"
            <> "Please report this to <https://github.com/mesabloo/nihil/issues> with details about what happened.\n"
            <> prettyCallStack callStack <> "\n"
    in errorWithoutStackTrace abstract
