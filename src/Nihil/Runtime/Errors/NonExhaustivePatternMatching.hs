module Nihil.Runtime.Errors.NonExhaustivePatternMatching where

import Text.PrettyPrint.ANSI.Leijen
import Prelude hiding ((<$>))

noMorePatterns :: Doc
noMorePatterns = text "No more patterns to check against. Your pattern matching might be inexhaustive."