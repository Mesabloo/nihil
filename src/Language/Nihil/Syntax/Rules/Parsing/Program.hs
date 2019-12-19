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

module Language.Nihil.Syntax.Rules.Parsing.Program where

import Language.Nihil.Syntax.Parser (Parser)
import Language.Nihil.Syntax.Internal.Parsing.AST
import Language.Nihil.Syntax.Internal.Parsing.Located
import Language.Nihil.Syntax.Internal.Parsing.Helpers (nonIndented)
import Language.Nihil.Syntax.Rules.Parsing.Symbol
import Language.Nihil.Syntax.Rules.Parsing.Toplevel.Function
import Language.Nihil.Syntax.Rules.Parsing.Toplevel.DataType
import Language.Nihil.Syntax.Rules.Parsing.Toplevel.TypeAlias
import Language.Nihil.Syntax.Rules.Parsing.Toplevel.OperatorFixity
import Text.Megaparsec (many, eof, (<?>), choice, try)

program :: Parser Program
program =  many statement <* (eof <?> "EOF") <?> "statements"

statement :: Parser (Located Statement)
statement = nonIndented (choice [ customOp <?> "custom operator definition"
                                , customDataType <?> "custom data type declaration"
                                , typeAlias <?> "type alias declaration"
                                , try declaration <?> "function declaration"
                                , definition <?> "function definition" ]) <* many (symbol ";")
