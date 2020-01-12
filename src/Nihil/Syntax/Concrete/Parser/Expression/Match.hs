{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BlockArguments #-}

module Nihil.Syntax.Concrete.Parser.Expression.Match
( pMatch ) where

import Nihil.Syntax.Common (Parser)
import Nihil.Syntax.Concrete.Core
import Nihil.Syntax.Concrete.Parser.Keyword
import Nihil.Syntax.Concrete.Parser
import Nihil.Syntax.Concrete.Parser.Enclosed
import Nihil.Utils.Source
import {-# SOURCE #-} Nihil.Syntax.Concrete.Parser.Expression
import Nihil.Syntax.Concrete.Parser.Pattern
import Nihil.Syntax.Concrete.Parser.Identifier
import Nihil.Syntax.Concrete.Debug
import qualified Text.Megaparsec as MP
import Control.Applicative ((<|>))

pMatch :: Parser Atom
pMatch = debug "pMatch" $ do
    pos <- getSourcePos
    pKeyword "match"
    expr <- sameLineOrIndented pos pExpression
    pKeyword "with"
    AMatch expr <$> (pBraces (pCases pos) <|> pCases pos)

pCases :: SourcePos -> Parser [([APattern], AExpr)]
pCases pos = sameLineOrIndented pos do
    pos' <- getSourcePos
    MP.some (sameLineOrColumn pos' pBranch <* MP.many (pSymbol ";"))

pBranch :: Parser ([APattern], AExpr)
pBranch = do
    pos <- getSourcePos
    pat <- pPattern
    sameLineOrIndented pos (pSymbol "->" <|> pSymbol "→")
    (pat, ) <$> sameLineOrIndented pos pExpression