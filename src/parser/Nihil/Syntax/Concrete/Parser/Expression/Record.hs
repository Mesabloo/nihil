{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Nihil.Syntax.Concrete.Parser.Expression.Record where

import Nihil.Syntax.Common (Parser)
import Nihil.Syntax.Concrete.Debug
import Nihil.Syntax.Concrete.Core
import Nihil.Syntax.Concrete.Parser.Enclosed
import Nihil.Syntax.Concrete.Parser.Statement.FunctionDeclaration
import Nihil.Syntax.Concrete.Parser.Identifier
import {-# SOURCE #-} Nihil.Syntax.Concrete.Parser.Expression.Atom
import qualified Text.Megaparsec as MP

pRecord :: Parser () -> Parser Atom
pRecord s = debug "p[Expr]Record" $ do
    ARecord <$> pBraces do
        s
        (pFunctionDefinition <* s) `MP.sepBy` (pSymbol' ";" <* s)

pRecordAccess :: Parser () -> Parser Atom
pRecordAccess s = debug "pRecordAccess" $ do
    ARecordAccess <$> pAtom s
                  <*> (pSymbol' "." *> s *> pIdentifier)
