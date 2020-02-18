{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Nihil.Syntax.Concrete.Parser.Statement.ClassDeclaration where

import Nihil.Syntax.Common (Parser)
import Nihil.Syntax.Concrete.Core
import Nihil.Syntax.Concrete.Parser
import Nihil.Syntax.Concrete.Parser.Keyword
import Nihil.Syntax.Concrete.Parser.Identifier
import Nihil.Syntax.Concrete.Parser.Statement.FunctionDeclaration
import Nihil.Syntax.Concrete.Debug
import qualified Text.Megaparsec as MP
import Control.Applicative ((<|>))

pTypeClass :: Parser AStatement
pTypeClass = debug "pTypeClass" $ withPosition do
    lineFold \s -> do
        pKeyword "class" <* MP.try s
        cls <- (,) <$> pIdentifier' <*> MP.some (MP.try (s *> pIdentifier))
        pKeyword "where"

        ClassDefinition cls <$> indentBlock (MP.try pFunctionDeclaration <|> pFunctionDefinition)