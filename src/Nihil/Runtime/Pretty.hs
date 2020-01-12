module Nihil.Runtime.Pretty
( -- * Re-exports
  putDoc
) where

import Nihil.Runtime.Core
import Text.PrettyPrint.ANSI.Leijen
import Prelude hiding ((<$>))

instance Pretty Value where
    pretty (VInteger i)           = integer i
    pretty (VDouble d)            = double d
    pretty (VCharacter c)         = text (show c)
    pretty (VId name)             = text name
    pretty (VTuple vs)            = tupled (fmap pretty vs)
    pretty (VConstructor name es) = text name <+> sep (fmap pretty es)
    pretty _                      = empty