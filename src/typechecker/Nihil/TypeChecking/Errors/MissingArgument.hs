module Nihil.TypeChecking.Errors.MissingArgument where

import Nihil.Utils.Source
import Text.PrettyPrint.ANSI.Leijen
import Prelude hiding ((<$>))
import Control.Arrow ((&&&))

{-| [Missing constructor pattern argument]

    Occurs when a constructor pattern does not have enough or has too many parameters.

    For example:

    > data Maybe a where
    >     Just    : a → Maybe a
    >     Nothing : Maybe a
    >
    > f : Maybe a → a
    > f Just = 0
    >
    > g : Maybe a → a
    > g (Nothing 0) = 0

    * Pattern matching against @Just@ requires /exactly/ 1 parameter, but none were given.

    * Pattern matching against @Nothing 0@ requires /exactly/ 0 parameters, but one was given.

    The only way to fix this error is to learn about pattern matching and data constructors.
-}
missingConstructorArgument :: Located String -> Int -> Int -> Doc
missingConstructorArgument name expected got =
    nest 4 (text "- Wrong number of arguments for a constructor pattern" <$> description)
  where description =
            let (loc, ann) = (location &&& annotated) name
            in text "> Expected number of arguments: " <> int expected <$>
               text "> Actual number of arguments:   " <> int got <$>
               text "> For constructor:" <> text ann <$>
               text "> At: " <> text (show loc) <> line