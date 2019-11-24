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

module Blob.Language.Syntax.Rules.Parsing.Expressions.Let where

import Blob.Language.Syntax.Internal.Parsing.Helpers
import Blob.Language.Syntax.Internal.Parsing.Located
import Blob.Language.Syntax.Internal.Parsing.AST
import Blob.Language.Syntax.Parser (Parser)
import Blob.Language.Syntax.Rules.Parsing.Keyword
import {-# SOURCE #-} Blob.Language.Syntax.Rules.Parsing.Expression
import {-# SOURCE #-} Blob.Language.Syntax.Rules.Parsing.Toplevel.Function
import Text.Megaparsec (some, try, (<|>))

let' :: Parser Atom
let' = do
    iPos <- getPositionAndIndent
    keyword "let"
    iPos2 <- getPositionAndIndent
    stts <- sameLineOrIndented iPos . some $ aligned iPos2 def
    sameIndentedOrLine iPos $ keyword "in"
    e <- sameLineOrIndented iPos expression
    pure $ ALet stts e
  where
    def :: Parser (Located Statement)
    def = try declaration <|> definition