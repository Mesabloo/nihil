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
pFunctionDeclaration = debug "pFunctionDeclaration" do
    withPosition do
        lineFold \s -> do
            name <- annotated <$> (pParens pAnySymbolᵉ <|> pIdentifier)
            MP.try s
            pSymbol' ":"
            MP.try s
            ty   <- pType s
            MP.try space1
            pure (FunDeclaration name ty)
    MP.<?> "function declaration"

pFunctionDefinition :: Parser AStatement
pFunctionDefinition = debug "pFunctionDefinition" do
    withPosition do
        lineFold \s -> do
            name <- annotated <$> (pParens pAnySymbolᵉ <|> pIdentifier)
            args <- Pattern.pAtom `MP.sepBy` MP.try s
            MP.try s
            pSymbol' "="
            MP.try s
            val  <- pExpression s
            MP.try space1
            pure (FunDefinition name args val)
    MP.<?> "function definition"
