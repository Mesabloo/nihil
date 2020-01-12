module Nihil.Syntax.Concrete.Parser.Expression.Let
( pLet ) where

import Nihil.Syntax.Common (Parser)
import Nihil.Syntax.Concrete.Parser
import Nihil.Syntax.Concrete.Parser.Keyword
import Nihil.Syntax.Concrete.Core
import Nihil.Utils.Source
import Nihil.Syntax.Concrete.Parser.Enclosed
import {-# SOURCE #-} Nihil.Syntax.Concrete.Parser.Expression
import Nihil.Syntax.Concrete.Parser.Statement.FunctionDeclaration
import Nihil.Syntax.Concrete.Debug
import qualified Text.Megaparsec as MP
import Control.Applicative ((<|>))

pLet :: Parser Atom
pLet = debug "pLet" $ do
    pos <- getSourcePos
    pKeyword "let"
    pos' <- getSourcePos
    stts <- sameLineOrIndented pos (pBraces (defs pos') <|> defs pos')
    pos <- getSourcePos
    pKeyword "in"
    ALet stts <$> sameLineOrIndented pos pExpression

defs :: SourcePos -> Parser [AStatement]
defs pos = MP.some (sameLineOrColumn pos def)

def :: Parser AStatement
def = MP.try pFunctionDeclaration <|> pFunctionDefinition