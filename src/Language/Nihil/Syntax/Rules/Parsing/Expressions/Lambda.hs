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

module Language.Nihil.Syntax.Rules.Parsing.Expressions.Lambda where

import Language.Nihil.Syntax.Parser (Parser)
import Language.Nihil.Syntax.Internal.Parsing.AST
import Language.Nihil.Syntax.Internal.Parsing.Helpers
import Language.Nihil.Syntax.Rules.Parsing.Symbol
import Language.Nihil.Syntax.Rules.Parsing.Patterns.Atom
import {-# SOURCE #-} Language.Nihil.Syntax.Rules.Parsing.Expression
import Control.Applicative (some, (<|>))

lambda :: Parser Atom
lambda = do
    iPos <- getPositionAndIndent
    symbol "\\" <|> symbol "λ"
    params <- some $ sameLineOrIndented iPos patTerm
    sameLineOrIndented iPos (symbol "->" <|> symbol "→")
    ALambda params <$> sameLineOrIndented iPos expression
