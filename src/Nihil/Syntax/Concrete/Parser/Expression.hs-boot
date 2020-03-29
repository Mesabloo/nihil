module Nihil.Syntax.Concrete.Parser.Expression where

import Nihil.Syntax.Common (Parser)
import Nihil.Syntax.Concrete.Core (Expr)

pExpression :: Parser () -> Parser Expr
