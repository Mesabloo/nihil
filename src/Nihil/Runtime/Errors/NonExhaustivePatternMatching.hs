module Nihil.Runtime.Errors.NonExhaustivePatternMatching where

import Text.PrettyPrint.ANSI.Leijen
import Prelude hiding ((<$>))

{-| [Non exhaustive pattern maching error]

    Occurs when a pattern matching does not cover all possible cases, and the value matched against falls into
    an uncovered case.

    /Please avoid partial pattern matching./
-}
noMorePatterns :: Doc
noMorePatterns = text "No more patterns to check against. Your pattern matching might be inexhaustive."