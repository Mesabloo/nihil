{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Nihil.Syntax.Concrete.Parser.Expression.Match
( pMatch ) where

import Nihil.Syntax.Common (Parser)
import Nihil.Syntax.Concrete.Core
import Nihil.Syntax.Concrete.Parser.Keyword
import Nihil.Syntax.Concrete.Parser
import {-# SOURCE #-} Nihil.Syntax.Concrete.Parser.Expression
import Nihil.Syntax.Concrete.Parser.Pattern
import Nihil.Syntax.Concrete.Parser.Identifier
import Nihil.Syntax.Concrete.Debug
import qualified Text.Megaparsec as MP
import Control.Applicative ((<|>))

pMatch :: Parser () -> Parser Atom
pMatch s = debug "pMatch" $ do
    pKeyword "match"
    expr <- s *> pExpression s
    s *> pKeyword "with"
    AMatch expr <$> indentBlock pBranch

pBranch :: Parser ([APattern], Expr)
pBranch = debug "pBranch" $ lexeme do
    lineFold \s -> do
        pat <- pPattern
        s *> (pSymbol' "->" <|> pSymbol' "→")
        (pat, ) <$> (s *> pExpression s)
