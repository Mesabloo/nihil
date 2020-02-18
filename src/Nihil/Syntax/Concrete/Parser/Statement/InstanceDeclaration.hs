{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}

module Nihil.Syntax.Concrete.Parser.Statement.InstanceDeclaration where

import Nihil.Syntax.Common (Parser)
import Nihil.Syntax.Concrete.Core
import Nihil.Syntax.Concrete.Debug
import Nihil.Syntax.Concrete.Parser
import Nihil.Syntax.Concrete.Parser.Keyword
import Nihil.Syntax.Concrete.Parser.Identifier
import Nihil.Syntax.Concrete.Parser.Statement.FunctionDeclaration
import qualified Nihil.Syntax.Concrete.Parser.Type.Atom as Type
import qualified Text.Megaparsec as MP
import Control.Applicative ((<|>))

pInstanceDeclaration :: Parser AStatement
pInstanceDeclaration = debug "pInstanceDeclaration" $ withPosition do
    lineFold \s -> do
        pKeyword "instance" <* MP.try s
        cls <- (,) <$> pIdentifier' <*> MP.some (MP.try (s *> Type.pAtom s))
        pKeyword "where"
        InstanceDefinition cls <$> indentBlock (MP.try pFunctionDeclaration <|> pFunctionDefinition)
