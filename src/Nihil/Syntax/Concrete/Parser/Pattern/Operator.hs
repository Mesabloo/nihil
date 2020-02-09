module Nihil.Syntax.Concrete.Parser.Pattern.Operator where

import Nihil.Syntax.Common (Parser)
import Nihil.Syntax.Concrete.Core
import Nihil.Syntax.Concrete.Parser.Identifier
import Nihil.Syntax.Concrete.Parser.Enclosed
import Nihil.Syntax.Concrete.Parser
import Nihil.Syntax.Concrete.Debug
import Nihil.Utils.Source

pOperator :: Parser APattern
pOperator = debug "p[Pattern]Operator" $ withPosition (POperator . annotated <$> pTicks pIdentifier')