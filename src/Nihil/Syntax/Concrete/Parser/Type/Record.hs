{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}

module Nihil.Syntax.Concrete.Parser.Type.Record
( pRecord ) where

import Nihil.Syntax.Common (Parser)
import Nihil.Syntax.Concrete.Debug
import Nihil.Syntax.Concrete.Core
import Nihil.Syntax.Concrete.Parser
import Nihil.Syntax.Concrete.Parser.Type.Row
import Nihil.Syntax.Concrete.Parser.Identifier
import Nihil.Syntax.Concrete.Parser.Keyword
import qualified Text.Megaparsec as MP
import Control.Applicative ((<|>))

pRecord :: Parser () -> Parser Type
pRecord s = debug "p[Type]Record" $ do
    (pKeyword "Π" <|> pSymbol' "*" <|> pSymbol' "×") <* MP.try s
    ext <- withPosition (pRow s)
    pure (TRecord ext)
