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
    params <- MP.some (MP.try (s *> Pattern.pAtom))
    MP.try s *> (pSymbol' "->" <|> MP.hidden (pSymbol' "→"))
    ALambda params <$> (MP.try s *> pExpression s)