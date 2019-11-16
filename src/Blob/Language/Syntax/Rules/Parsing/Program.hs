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

module Blob.Language.Syntax.Rules.Parsing.Program where

import Blob.Language.Syntax.Parser (Parser)
import Blob.Language.Syntax.Internal.Parsing.AST
import Blob.Language.Syntax.Internal.Parsing.Located
import Blob.Language.Syntax.Internal.Parsing.Helpers (nonIndented)
import Blob.Language.Syntax.Rules.Parsing.Symbol
import Blob.Language.Syntax.Rules.Parsing.Toplevel.Function
import Blob.Language.Syntax.Rules.Parsing.Toplevel.DataType
import Blob.Language.Syntax.Rules.Parsing.Toplevel.TypeAlias
import Blob.Language.Syntax.Rules.Parsing.Toplevel.OperatorFixity
import Text.Megaparsec (many, eof, (<?>), choice, try)

program :: Parser Program
program =  many statement <* (eof <?> "EOF") <?> "statements"

statement :: Parser (Located Statement)
statement = nonIndented (choice [ customOp <?> "custom operator definition"
                                , customDataType <?> "custom data type declaration"
                                , typeAlias <?> "type alias declaration"
                                , try declaration <?> "function declaration"
                                , definition <?> "function definition" ]) <* many (symbol ";")
