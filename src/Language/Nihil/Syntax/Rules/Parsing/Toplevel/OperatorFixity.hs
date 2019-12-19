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

module Language.Nihil.Syntax.Rules.Parsing.Toplevel.OperatorFixity where

import Language.Nihil.Syntax.Parser (Parser)
import Language.Nihil.Syntax.Internal.Parsing.Located
import Language.Nihil.Syntax.Internal.Parsing.AST
import Language.Nihil.Syntax.Internal.Parsing.Helpers
import Language.Nihil.Syntax.Rules.Parsing.Keyword
import Language.Nihil.Syntax.Rules.Parsing.Symbol
import Language.Nihil.Syntax.Rules.Parsing.Literal
import Language.Nihil.Syntax.Rules.Parsing.Identifier
import Text.Megaparsec (choice, (<?>))
import Control.Monad (guard)
import Control.Applicative ((<|>))

customOp :: Parser (Located Statement)
customOp = do
    (span', op) <- getPositionInSource $ do
        iPos <- getPositionAndIndent
        f <- fixity
        prec <- sameLineOrIndented iPos integer <?> "operator precedence"
        guard (prec <= 9) <|> fail "Operator precedence should be < 10 and > 0"

        op <- sameLineOrIndented iPos (ticks identifier <|> opSymbol <|> parens opSymbol) <?> "operator"

        pure $ OpFixity op (f prec op :@ Nothing)

    pure (op :@ Just span')
  where
    fixity = Infix <$> choice [ L <$ keyword "infixl"
                              , R <$ keyword "infixr" ]
