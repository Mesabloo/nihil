module Nihil.Syntax.Concrete.Parser.Expression.Lambda where

import Nihil.Syntax.Common (Parser)
import Nihil.Syntax.Concrete.Core
import Nihil.Syntax.Concrete.Parser
import Nihil.Syntax.Concrete.Parser.Identifier
import qualified Nihil.Syntax.Concrete.Parser.Pattern.Atom as Pattern
import {-# SOURCE #-} Nihil.Syntax.Concrete.Parser.Expression
import Nihil.Syntax.Concrete.Debug
import Control.Applicative ((<|>))
import qualified Text.Megaparsec as MP

pLambda :: Parser Atom
pLambda = debug "pLambda" $ do
    pos <- getSourcePos
    pSymbol "\\" <|> pSymbol "λ"
    params <- MP.some (sameLineOrIndented pos Pattern.pAtom)
    sameLineOrIndented pos (pSymbol "->" <|> pSymbol "→")
    ALambda params <$> sameLineOrIndented pos pExpression