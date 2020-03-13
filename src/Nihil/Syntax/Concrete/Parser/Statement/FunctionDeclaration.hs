{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Nihil.Syntax.Concrete.Parser.Statement.FunctionDeclaration where

import Nihil.Syntax.Common (Parser)
import Nihil.Syntax.Concrete.Core
import Nihil.Syntax.Concrete.Parser
import Nihil.Syntax.Concrete.Parser.Enclosed
import Nihil.Syntax.Concrete.Parser.Identifier
import {-# SOURCE #-} Nihil.Syntax.Concrete.Parser.Type
import qualified Nihil.Syntax.Concrete.Parser.Pattern.Atom as Pattern
import {-# SOURCE #-} Nihil.Syntax.Concrete.Parser.Expression
import Nihil.Syntax.Concrete.Debug
import Nihil.Utils.Source
import Control.Applicative ((<|>))
import qualified Text.Megaparsec as MP

pFunctionDeclaration :: Parser AStatement
pFunctionDeclaration = debug "pFunctionDeclaration" $ withPosition do
    lineFold \s -> do
        name <- annotated <$> (pParens pAnySymbolᵉ <|> pIdentifier)
        MP.try s *> pSymbol' ":"
        ty   <- MP.try s *> pType s
        pure (FunDeclaration name ty)

pFunctionDefinition :: Parser AStatement
pFunctionDefinition = debug "pFunctionDefinition" $ withPosition do
    lineFold \s -> do
        name <- annotated <$> (pParens pAnySymbolᵉ <|> pIdentifier)
        args <- MP.many (MP.try (s *> Pattern.pAtom))
        MP.try s *> pSymbol' "="
        val  <- MP.try s *> pExpression s
        pure (FunDefinition name args val)
