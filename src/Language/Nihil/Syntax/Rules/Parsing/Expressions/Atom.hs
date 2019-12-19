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

module Language.Nihil.Syntax.Rules.Parsing.Expressions.Atom where

import Language.Nihil.Syntax.Parser (Parser)
import Language.Nihil.Syntax.Internal.Parsing.AST
import Language.Nihil.Syntax.Internal.Parsing.Helpers
import Language.Nihil.Syntax.Internal.Parsing.Located
import Language.Nihil.Syntax.Rules.Parsing.Identifier
import Language.Nihil.Syntax.Rules.Parsing.Literal
import Language.Nihil.Syntax.Rules.Parsing.Wildcard
import Language.Nihil.Syntax.Rules.Parsing.Symbol
import {-# SOURCE #-} Language.Nihil.Syntax.Rules.Parsing.Expression
import Language.Nihil.Syntax.Rules.Parsing.Expressions.Lambda
import Language.Nihil.Syntax.Rules.Parsing.Expressions.Match
import Language.Nihil.Syntax.Rules.Parsing.Expressions.Tuple
import Language.Nihil.Syntax.Rules.Parsing.Expressions.List
import Language.Nihil.Syntax.Rules.Parsing.Expressions.Let
import Language.Nihil.Syntax.Rules.Parsing.Expressions.Operator
import Language.Nihil.Syntax.Rules.Parsing.Expressions.Application
import Text.Megaparsec (try, (<|>), choice, (<?>))

atom :: Parser (Located Atom)
atom = operator <|> expr
  where expr = try app <|> exprNoApp

exprNoApp :: Parser (Located Atom)
exprNoApp = do
    (span', a) <- getPositionInSource $
        choice [ hole <?> "type hole", lambda <?> "lambda", match <?> "match"
               , try tuple <?> "tuple", list <?> "list", let' <?> "let expression"
               , AId <$> choice [ identifier, try (parens opSymbol), typeIdentifier ] <?> "identifier"
               , ALit . LDec <$> float <?> "floating point number"
               , ALit . LInt <$> integer <?> "integer"
               , ALit . LChr <$> char <?> "character"
               , ALit . LStr <$> string <?> "string"
               , AParens <$> parens expression <?> "parenthesized expression" ]

    pure $ a :@ Just span'