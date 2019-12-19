-- The Great Nihil Compiler
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

module Language.Nihil.Syntax.Rules.Parsing.Types.Arrow where

import Language.Nihil.Syntax.Parser (Parser)
import Language.Nihil.Syntax.Internal.Parsing.Located
import Language.Nihil.Syntax.Internal.Parsing.AST
import Language.Nihil.Syntax.Internal.Parsing.Helpers
import Language.Nihil.Syntax.Rules.Parsing.Symbol
import Language.Nihil.Syntax.Rules.Parsing.Literal
import Language.Nihil.Syntax.Rules.Parsing.Types.Application
import Text.Megaparsec (optional, between, (<|>))

type' :: Parser (Located Type)
type' = do
    iPos <- getPositionAndIndent
    (span', (ty, optTy)) <- getPositionInSource $ do
        ft <- btype
        ot <- optional $ do
            usage <- sameLineOrIndented iPos (symbol "#" {- keep this for now; parsing seems to be quite difficult -} *> integer) -- (between (symbol "|") (symbol "|") integer)
            sameLineOrIndented iPos (symbol "->" <|> symbol "→")
            (usage,) <$> sameLineOrIndented iPos type'
        pure (ft, ot)

    case optTy of
        Nothing -> pure ty
        Just (usage, ty') -> pure (TFun (ty, usage) ty' :@ Just span')
