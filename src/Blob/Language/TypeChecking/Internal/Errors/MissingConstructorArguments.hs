module Blob.Language.TypeChecking.Internal.Errors.MissingConstructorArguments where

import Text.PrettyPrint.ANSI.Leijen

makeMissingConstructorPatternArgumentError :: String -> Int -> Int -> Doc
makeMissingConstructorPatternArgumentError id' expected actual =
    text "- Not enough arguments for constructor pattern \"" <> text id' <> text "\":" <> linebreak
    <> text "  > Expected number of arguments: " <> int expected <> linebreak
    <> text "  > Actual number of arguments:   " <> int actual <> linebreak