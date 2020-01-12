module Nihil.Syntax.Abstract.Desugarer.Expression where

import qualified Nihil.Syntax.Concrete.Core as CC
import qualified Nihil.Syntax.Abstract.Core as AC
import Nihil.Syntax.Common (Desugarer)

desugarExpression :: CC.AExpr -> Desugarer AC.Expr