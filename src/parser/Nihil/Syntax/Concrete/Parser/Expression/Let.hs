{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Nihil.Syntax.Concrete.Parser.Expression.Let
( pLet ) where

import Nihil.Syntax.Common (Parser)
import Nihil.Syntax.Concrete.Parser
import Nihil.Syntax.Concrete.Parser.Keyword
import Nihil.Syntax.Concrete.Core
import {-# SOURCE #-} Nihil.Syntax.Concrete.Parser.Expression
import Nihil.Syntax.Concrete.Parser.Statement.FunctionDeclaration
import Nihil.Syntax.Concrete.Debug
import qualified Text.Megaparsec as MP
import Control.Applicative ((<|>))

pLet :: Parser () -> Parser Atom
pLet s = debug "pLet" $ do
    pKeyword "let"
    stts <- indentBlock def
    s *> pKeyword "in"
    ALet stts <$> (s *> pExpression s)

def :: Parser AStatement
def = MP.try pFunctionDeclaration <|> pFunctionDefinition
