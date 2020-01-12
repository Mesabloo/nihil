{-# LANGUAGE BlockArguments #-}

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
import Control.Applicative ((<|>))
import qualified Text.Megaparsec as MP

pFunctionDeclaration :: Parser AStatement
pFunctionDeclaration = debug "pFunctionDeclaration" $ withPosition do
    pos  <- getSourcePos
    name <- pParens pAnySymbolᵉ <|> pIdentifier
    sameLineOrIndented pos (pSymbol ":")
    ty   <- sameLineOrIndented pos pType
    pure (FunDeclaration name ty)

pFunctionDefinition :: Parser AStatement
pFunctionDefinition = debug "pFunctionDefinition" $ withPosition do
    pos  <- getSourcePos
    name <- pParens pAnySymbolᵉ <|> pIdentifier
    args <- MP.many (sameLineOrIndented pos Pattern.pAtom)
    sameLineOrIndented pos (pSymbol "=")
    val  <- sameLineOrIndented pos pExpression
    pure (FunDefinition name args val)