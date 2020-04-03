{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Nihil.Syntax.Concrete.Parser.Statement.OperatorFixity where

import Nihil.Syntax.Common (Parser)
import Nihil.Syntax.Concrete.Core
import Nihil.Syntax.Concrete.Parser
import Nihil.Syntax.Concrete.Parser.Enclosed
import Nihil.Syntax.Concrete.Parser.Identifier
import Nihil.Syntax.Concrete.Parser.Literal
import Nihil.Syntax.Concrete.Parser.Keyword
import Nihil.Utils.Source
import Nihil.Utils.Annotation
import Nihil.Syntax.Concrete.Debug
import Control.Applicative ((<|>))
import Data.Bifunctor (first)
import qualified Text.Megaparsec as MP

pOperatorFixity :: Parser AStatement
pOperatorFixity = debug "pOperatorFixity" $ withPosition do
    lineFold \s -> do
        f             <- withPosition fixity
        LInteger prec <- annotated <$> (s *> pInteger)
        operator      <- annotated <$> (s *> (pParens op <|> op))
        pure (OperatorFixity operator (hoistAnnotated (first ($ prec)) f))
  where op     = pTicks pIdentifier <|> pAnySymbolᵉ <|> pAnySymbolᵗ
        fixity = Infix <$> MP.choice
            [ L <$ pKeyword "infixl"
            , R <$ pKeyword "infixr" ]
