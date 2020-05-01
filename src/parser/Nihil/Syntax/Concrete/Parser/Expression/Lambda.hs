{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Nihil.Syntax.Concrete.Parser.Expression.Lambda where

import Nihil.Syntax.Common (Parser)
import Nihil.Syntax.Concrete.Core
import Nihil.Syntax.Concrete.Parser.Identifier
import qualified Nihil.Syntax.Concrete.Parser.Pattern.Atom as Pattern
import {-# SOURCE #-} Nihil.Syntax.Concrete.Parser.Expression
import Nihil.Syntax.Concrete.Debug
import Control.Applicative ((<|>))
import qualified Text.Megaparsec as MP

pLambda :: Parser () -> Parser Atom
pLambda s = debug "pLambda" $ do
    pSymbol' "\\" <|> MP.hidden (pSymbol' "λ")
    s
    params <- Pattern.pAtom `MP.sepEndBy1` s
    pSymbol' "->" <|> MP.hidden (pSymbol' "→")
    ALambda params <$> (s *> pExpression s)
