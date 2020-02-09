{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Nihil.Syntax.Concrete.Parser.Expression.Tuple where

import Nihil.Syntax.Common (Parser)
import Nihil.Syntax.Concrete.Core
import Nihil.Syntax.Concrete.Parser
import Nihil.Syntax.Concrete.Parser.Enclosed
import {-# SOURCE #-} Nihil.Syntax.Concrete.Parser.Expression
import Nihil.Syntax.Concrete.Parser.Identifier
import Nihil.Syntax.Concrete.Debug
import Control.Applicative ((<|>))
import qualified Text.Megaparsec as MP

pTuple :: Parser () -> Parser Atom
pTuple s = debug "p[Expression]Tuple" $ lexeme do
    MP.try unit <|> tuple
  where unit = ATuple [] <$ pParens (pure ())
        tuple = ATuple <$> p

        p = do
            pParens (lexemeN (pExpression s) `sepBy2` lexemeN (pSymbol' ","))

        sepBy2 p sep = do
            (:) <$> (p <* sep) <*> (p `MP.sepBy1` sep)