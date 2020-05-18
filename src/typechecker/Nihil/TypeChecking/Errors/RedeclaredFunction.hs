module Nihil.TypeChecking.Errors.RedeclaredFunction where

import Nihil.Utils.Source
import Text.PrettyPrint.ANSI.Leijen
import Prelude hiding ((<$>))
import Control.Arrow ((&&&))

{-| [Redeclared function error]

    Occurs when a function has multiple type signatures to unify with, making typechecking impossible.

    For example:

    > f :: Integer
    > f :: Char
    > f = 0

    How would the compiler choose between both type signatures? 50% chance for each? But then typechecking would be random, thus not working properly.
-}
redeclaredFunction :: Located String -> Doc
redeclaredFunction name = nest 4 (text "- Redeclared function" <$> description)
  where description =
            let (ann, loc) = (annotated &&& location) name
            in text "> For function named: " <> text ann <$>
               text "> At: " <> text (show loc) <> line