module Nihil.Syntax.Concrete.Parser.Pattern.Tuple where

import Nihil.Syntax.Common (Parser)
import Nihil.Syntax.Concrete.Core
import Nihil.Syntax.Concrete.Parser
import Nihil.Syntax.Concrete.Parser.Enclosed
import Nihil.Syntax.Concrete.Parser.Identifier
import Nihil.Syntax.Concrete.Debug
import {-# SOURCE #-} Nihil.Syntax.Concrete.Parser.Pattern
import Control.Applicative ((<|>))
import qualified Text.Megaparsec as MP

pTuple :: Parser Pattern
pTuple = debug "p[Pattern]Tuple" $ do
    pos <- getSourcePos
    MP.try unit <|> tuple pos
  where unit = PTuple [] <$ pParens (pure ())
        tuple pos = PTuple <$> pParens ((:) <$> sameLineOrIndented pos pPattern
                                            <*> MP.some (sameLineOrIndented pos (pSymbol ",") *> sameLineOrIndented pos pPattern))