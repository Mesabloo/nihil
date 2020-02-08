{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

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
    MP.try unit <|> tuple
  where unit  = TTuple [] <$ pParens (pure ())
        tuple = TTuple <$> p

        p = lineFold \spaceConsumer -> do
            pParens (pType `sepBy2` (MP.between (MP.try spaceConsumer) (MP.try spaceConsumer) (pSymbol' ",")))

        sepBy2 p sep = do
            (:) <$> (p <* sep) <*> (p `MP.sepBy1` sep)
