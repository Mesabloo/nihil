{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Nihil.Syntax.Concrete.Parser.Expression.Where where

import Nihil.Syntax.Common (Parser)
import Nihil.Syntax.Concrete.Core
import Nihil.Syntax.Concrete.Parser
import Nihil.Syntax.Concrete.Parser.Keyword
import Nihil.Syntax.Concrete.Parser.Statement.FunctionDeclaration
import Nihil.Syntax.Concrete.Debug
import qualified Text.Megaparsec as MP
import Control.Applicative ((<|>))

pWhere :: Parser () -> Parser [AStatement]
pWhere s = debug "pWhere" $ do
    indentBlock do
        pKeyword "where"
        pure (IndentSome Nothing pure def)
  where def = MP.try pFunctionDeclaration <|> pFunctionDefinition