{-# LANGUAGE OverloadedStrings #-}

module Nihil.Syntax.Concrete.Parser.Enclosed where

import Nihil.Syntax.Common (Parser)
import Nihil.Syntax.Concrete.Parser.Identifier (pSymbol')
import qualified Text.Megaparsec as MP

pTicks :: Parser a -> Parser a
pTicks = MP.between (pSymbol' "`") (pSymbol' "`")

pParens :: Parser a -> Parser a
pParens = MP.between (pSymbol' "(") (pSymbol' ")")

pBraces :: Parser a -> Parser a
pBraces = MP.between (pSymbol' "{") (pSymbol' "}")