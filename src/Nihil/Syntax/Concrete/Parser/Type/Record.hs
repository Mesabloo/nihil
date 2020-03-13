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
import Nihil.Syntax.Concrete.Parser.Statement.FunctionDeclaration
import qualified Text.Megaparsec as MP

pRecord :: Parser () -> Parser Type
pRecord s = debug "p[Type]Record" $ do
    (fields, extended) <- pBraces do
        MP.try s
        fields <- ((pFunctionDeclaration <* MP.try s) `MP.sepBy1` (pSymbol' ";" <* MP.try s))
        ext <- MP.optional do
            withPosition do
                MP.try s
                pSymbol' "|"
                (TVar . annotated <$> pIdentifier) <* MP.try s
        pure (fields, ext)
    pure (TRecord fields extended)
