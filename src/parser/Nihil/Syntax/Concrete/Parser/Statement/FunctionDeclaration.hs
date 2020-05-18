{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Nihil.Syntax.Concrete.Parser.Statement.FunctionDeclaration where

import Nihil.Syntax.Common (Parser)
import Nihil.Syntax.Concrete.Core
import Nihil.Syntax.Concrete.Parser
import Nihil.Syntax.Concrete.Parser.Enclosed
import Nihil.Syntax.Concrete.Parser.Identifier
import Nihil.Syntax.Concrete.Parser.Type
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
            s
            pSymbol' ":"
            s
            ty   <- lexeme (pType s)
            pure (FunDeclaration name ty)
    MP.<?> "function declaration"

pFunctionDefinition :: Parser AStatement
pFunctionDefinition = debug "pFunctionDefinition" do
    withPosition do
        lineFold \s -> do
            name <- annotated <$> (pParens pAnySymbolᵉ <|> pIdentifier)
            args <- Pattern.pAtom `MP.sepBy` s
            s
            pSymbol' "="
            s
            val  <- lexeme (pExpression s)
            pure (FunDefinition name args val)
    MP.<?> "function definition"
