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

module Blob.Language.Syntax.Rules.Parsing.Toplevel.Function where

import Blob.Language.Syntax.Parser (Parser)
import Blob.Language.Syntax.Internal.Parsing.Located
import Blob.Language.Syntax.Internal.Parsing.AST
import Blob.Language.Syntax.Internal.Parsing.Helpers
import Blob.Language.Syntax.Rules.Parsing.Symbol
import Blob.Language.Syntax.Rules.Parsing.Identifier
import Blob.Language.Syntax.Rules.Parsing.Types.Arrow
import Blob.Language.Syntax.Rules.Parsing.Patterns.Atom
import {-# SOURCE #-} Blob.Language.Syntax.Rules.Parsing.Expression
import Control.Applicative (many, (<|>))

declaration :: Parser (Located Statement)
declaration = do
    (span', decl) <- getPositionInSource $ do
        iPos <- getPositionAndIndent
        name <- identifier <|> parens opSymbol
        sameLineOrIndented iPos (symbol "::" <|> symbol "∷")
        t <- sameLineOrIndented iPos type'

        pure (Declaration name t)
    pure (decl :@ Just span')

definition :: Parser (Located Statement)
definition = do
    (span', def) <- getPositionInSource $ do
        iPos <- getPositionAndIndent
        name <- identifier <|> parens opSymbol
        args <- many $ sameLineOrIndented iPos patTerm
        sameLineOrIndented iPos (symbol "=")
        v <- sameLineOrIndented iPos expression

        pure (Definition name args v)
    pure (def :@ Just span')