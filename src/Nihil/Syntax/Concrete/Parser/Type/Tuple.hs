module Nihil.Syntax.Concrete.Parser.Type.Tuple where

import Nihil.Syntax.Common (Parser)
import Nihil.Syntax.Concrete.Core
import Nihil.Syntax.Concrete.Parser
import Nihil.Syntax.Concrete.Parser.Enclosed
import Nihil.Syntax.Concrete.Parser.Identifier
import {-# SOURCE #-} Nihil.Syntax.Concrete.Parser.Type
import Nihil.Syntax.Concrete.Debug
import qualified Text.Megaparsec as MP
import Control.Applicative ((<|>))

pTuple :: Parser Type
pTuple = debug "p[Type]Tuple" $ do
    pos <- getSourcePos
    unit <|> tuple pos
  where unit      = TTuple [] <$ pParens (pure ())
        tuple pos = TTuple <$> pParens ((:) <$> sameLineOrIndented pos pType
                                            <*> MP.some (sameLineOrIndented pos (pSymbol ",") *> sameLineOrIndented pos pType))