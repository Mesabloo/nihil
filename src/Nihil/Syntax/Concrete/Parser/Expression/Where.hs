module Nihil.Syntax.Concrete.Parser.Expression.Where where

import Nihil.Syntax.Common (Parser)
import Nihil.Syntax.Concrete.Core
import Nihil.Syntax.Concrete.Parser
import Nihil.Syntax.Concrete.Parser.Keyword
import Nihil.Syntax.Concrete.Parser.Enclosed
import Nihil.Syntax.Concrete.Parser.Identifier
import Nihil.Syntax.Concrete.Parser.Statement.FunctionDeclaration
import Nihil.Syntax.Concrete.Debug
import qualified Text.Megaparsec as MP
import Control.Applicative ((<|>))

pWhere :: Parser [AStatement]
pWhere = debug "pWhere" $ do
    pKeyword "where"
    pos <- getSourcePos
    pBraces (defs pos) <|> (defs pos)
  where defs pos = MP.some (sameColumn pos def <* MP.many (pSymbol ";"))
        def      = MP.try pFunctionDeclaration <|> pFunctionDefinition