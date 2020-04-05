{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}

module Nihil.Syntax.Concrete.Parser.Type.Row where

import Nihil.Syntax.Common (Parser)
import Nihil.Syntax.Concrete.Debug
import Nihil.Syntax.Concrete.Core
import Nihil.Utils.Source
import Nihil.Syntax.Concrete.Parser
import Nihil.Syntax.Concrete.Parser.Statement.FunctionDeclaration
import Nihil.Syntax.Concrete.Parser.Identifier
import Nihil.Syntax.Concrete.Parser.Enclosed
import qualified Text.Megaparsec as MP

pRow :: Parser () -> Parser Type
pRow s = debug "pRow" $ do
    (fields, ext) <- pBraces do
        s
        fields <- (pFunctionDeclaration <* s) `MP.sepBy` (pSymbol' ";" <* s)
        let pExt = do
                pSymbol' "|"
                TVar . annotated <$> pIdentifier <* s
        ext <- MP.optional (withPosition pExt)
        pure (fields, ext)
    pure (TRow fields ext)
