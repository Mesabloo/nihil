{-# LANGUAGE OverloadedStrings #-}

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
    MP.try unit <|> tuple
  where unit = PTuple [] <$ pParens (pure ())
        tuple = PTuple <$>
            pParens (lexeme pPattern `sepBy2` lexeme (pSymbol' ","))

        sepBy2 p sep = do
            (:) <$> (p <* sep) <*> (p `MP.sepBy1` sep)

        -- pParens ((:) <$> sameLineOrIndented pos pPattern
        --                                     <*> MP.some (sameLineOrIndented pos (pSymbol ",") *> sameLineOrIndented pos pPattern))
