module Blob.Language.PrettyPrinting.Pretty where

import Text.PrettyPrint.Leijen (Doc)

class Pretty a where
    pretty :: a -> Doc