{-# LANGUAGE BlockArguments #-}

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
import Nihil.Syntax.Concrete.Parser.Identifier
import Nihil.Syntax.Concrete.Debug
import qualified Text.Megaparsec as MP
import Control.Applicative ((<|>))
import Control.Monad (when, void)

pLet :: Parser Atom
pLet = debug "pLet" $ do
    pos <- getSourcePos
    pKeyword "let"
    pos' <- getSourcePos
    stts <- sameLineOrIndented pos (pBraces (defs pos' True) <|> defs pos' False)
    pos <- getSourcePos
    pKeyword "in"
    ALet stts <$> sameLineOrIndented pos pExpression

defs :: SourcePos -> Bool -> Parser [AStatement]
defs pos hasBraces = MP.some (sameLineOrColumn pos def <* when hasBraces do void (MP.many (pSymbol ";")))

def :: Parser AStatement
def = MP.try pFunctionDeclaration <|> pFunctionDefinition