module Nihil.Syntax.Abstract.Desugarer.Type where

import qualified Nihil.Syntax.Concrete.Core as CC (AType)
import qualified Nihil.Syntax.Abstract.Core as AC (Type)
import Nihil.Syntax.Common (Desugarer)

desugarType :: [CC.AType] -> Desugarer AC.Type
