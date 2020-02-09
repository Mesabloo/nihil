{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

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
    lineFold \s -> indentBlock do
        pKeyword "match"
        expr <- MP.try s *> pExpression
        MP.try s *> pKeyword "with"
        pure (IndentSome Nothing (pure . AMatch expr) pBranch)

pBranch :: Parser ([APattern], AExpr)
pBranch = lexeme do
    lineFold \s -> do
        pat <- pPattern
        MP.try s *> (pSymbol' "->" <|> pSymbol' "→")
        (pat, ) <$> (MP.try s *> pExpression)