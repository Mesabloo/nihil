{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}

module Nihil.Syntax.Concrete.Parser.Type.Record
( pRecord ) where

import Nihil.Syntax.Common (Parser)
import Nihil.Utils.Source
import Nihil.Syntax.Concrete.Debug
import Nihil.Syntax.Concrete.Core
import Nihil.Syntax.Concrete.Parser
import Nihil.Syntax.Concrete.Parser.Enclosed
import Nihil.Syntax.Concrete.Parser.Identifier
import {-# SOURCE #-} Nihil.Syntax.Concrete.Parser.Type
import qualified Text.Megaparsec as MP

pRecord :: Parser Type
pRecord = debug "pRecord" $ lineFold \s -> do
    (fields, extended) <- pBraces do
        fields <- ((pField <* MP.try s) `MP.sepBy1` (pSymbol' ";" <* MP.try s))
        ext <- MP.optional do
            withPosition do
                MP.try s
                pSymbol' "|"
                (TVar . annotated <$> pIdentifier) <* MP.try s
        pure (fields, ext)
    pure (TRecord fields extended)

pField :: Parser (Located String, [AType])
pField = lineFold \s -> do
    name <- pIdentifier
    MP.try s *> pSymbol' ":"
    ty <- MP.try s *> pType s
    pure (name, ty)
