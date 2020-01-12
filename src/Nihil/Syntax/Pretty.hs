{-# OPTIONS_GHC -Wno-unused-imports #-}

module Nihil.Syntax.Pretty
( -- * Re-exports
  module Nihil.Syntax.Pretty.Concrete
, module Nihil.Syntax.Pretty.Abstract
, putDoc
) where

import Nihil.Syntax.Pretty.Concrete
import Nihil.Syntax.Pretty.Abstract
import Text.PrettyPrint.ANSI.Leijen (putDoc)