{-# LANGUAGE OverloadedStrings #-}

module Nihil.Syntax.Concrete.Parser.Type.Operator where

import Nihil.Syntax.Common (Parser)
import Nihil.Syntax.Concrete.Core
import Nihil.Syntax.Concrete.Parser
import Nihil.Syntax.Concrete.Parser.Identifier
import Nihil.Syntax.Concrete.Debug
import Nihil.Utils.Source
import qualified Text.Megaparsec as MP
import Control.Applicative ((<|>))

pOperator :: Parser AType
pOperator = debug "p[Type]Operator" $ withPosition ((TOperator . annotated <$> pAnySymbolᵗ) <|> otherSymbols)
  where otherSymbols =
            TOperator <$> MP.choice
                [ "->" <$ pSymbol' "->"
                , "->" <$ pSymbol' "→"
                , "=>" <$ pSymbol' "=>"
                , "=>" <$ pSymbol' "⇒"
                , "\\" <$ pSymbol' "\\"
                ]
