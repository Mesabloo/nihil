-- Blobc, a compiler for compiling Blob source code
-- Copyright (c) 2019 Mesabloo

-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.

-- You should have received a copy of the GNU General Public License
-- along with this program. If not, see <http://www.gnu.org/licenses/>.

{-# LANGUAGE TupleSections #-}

module Blob.Language.Syntax.Rules.Parsing.Types.Arrow where

import Blob.Language.Syntax.Parser (Parser)
import Blob.Language.Syntax.Internal.Parsing.Located
import Blob.Language.Syntax.Internal.Parsing.AST
import Blob.Language.Syntax.Internal.Parsing.Helpers
import Blob.Language.Syntax.Rules.Parsing.Symbol
import Blob.Language.Syntax.Rules.Parsing.Literal
import Blob.Language.Syntax.Rules.Parsing.Types.Application
import Text.Megaparsec (optional, between, (<|>))

type' :: Parser (Located Type)
type' = do
    iPos <- getPositionAndIndent
    (span', (ty, optTy)) <- getPositionInSource $ do
        ft <- btype
        ot <- optional $ do
            usage <- sameLineOrIndented iPos (between (symbol "|") (symbol "|") integer)
            sameLineOrIndented iPos (symbol "->" <|> symbol "→")
            (usage,) <$> sameLineOrIndented iPos type'
        pure (ft, ot)

    case optTy of
        Nothing -> pure ty
        Just (usage, ty') -> pure (TFun (ty, usage) ty' :@ Just span')
