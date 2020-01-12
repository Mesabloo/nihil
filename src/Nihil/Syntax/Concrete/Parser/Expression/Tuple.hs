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

pTuple :: Parser Atom
pTuple = debug "p[Expression]Tuple" $ do
    pos <- getSourcePos
    unit <|> tuple pos
  where unit      = ATuple [] <$ pParens (pure ())
        tuple pos = ATuple <$> pParens ((:) <$> sameLineOrIndented pos pExpression
                                            <*> MP.some (sameLineOrIndented pos (pSymbol ",") *> sameLineOrIndented pos pExpression))