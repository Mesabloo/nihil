module Blob.Desugaring.Errors where

import Text.PrettyPrint.Leijen

makeUnexpectedOperatorError :: String -> Doc
makeUnexpectedOperatorError o = text "Unexpected infix operator “" <> text o <> text "”" <> dot <> linebreak