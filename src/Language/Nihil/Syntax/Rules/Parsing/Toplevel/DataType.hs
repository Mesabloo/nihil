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

module Language.Nihil.Syntax.Rules.Parsing.Toplevel.DataType where

import Language.Nihil.Syntax.Internal.Parsing.Located
import Language.Nihil.Syntax.Internal.Parsing.AST
import Language.Nihil.Syntax.Internal.Parsing.Helpers
import Language.Nihil.Syntax.Rules.Parsing.Keyword
import Language.Nihil.Syntax.Rules.Parsing.Symbol
import Language.Nihil.Syntax.Rules.Parsing.Identifier
import Language.Nihil.Syntax.Rules.Parsing.Types.Arrow
import Language.Nihil.Syntax.Rules.Parsing.Types.Atom
import Language.Nihil.Syntax.Parser (Parser)
import qualified Data.Map as Map
import Control.Applicative ((<|>), many)
import Text.Megaparsec (choice)

customDataType :: Parser (Located Statement)
customDataType = do
    (span', dataType) <- getPositionInSource $ do
        keyword "data"
        iPos <- getPositionAndIndent
        name <- sameLineOrIndented iPos typeIdentifier
        tvs <- many $ sameLineOrIndented iPos identifier

        TypeDeclaration name tvs <$> choice [ sameLineOrIndented iPos (symbol "=") *> adtDeclaration iPos
                                            , sameLineOrIndented iPos (keyword "where") *> gadtDeclaration iPos ]

    pure (dataType :@ Just span')
  where
    adtDeclaration iPos = do
        (span', cType) <- getPositionInSource $ do
            let ctor = (,) <$> typeIdentifier <*> many (sameLineOrIndented iPos atype)
            ctor1 <- sameLineOrIndented iPos ctor
            ctors <- many (sameLineOrIndented iPos (symbol "|") *> sameLineOrIndented iPos ctor)
            pure (TSum $ Map.fromList (ctor1:ctors))
        pure (cType :@ Just span')

    gadtDeclaration iPos = do
        (span', cType) <- getPositionInSource $ do
            let ctor =
                    (,) <$> typeIdentifier
                        <*> (sameLineOrIndented iPos (symbol "::" <|> symbol "∷") *> sameLineOrIndented iPos type')
            ctor1 <- sameLineOrIndented iPos ctor
            ctors <- many (sameLineOrIndented iPos (symbol "|") *> sameLineOrIndented iPos ctor)
            pure (TGADT $ Map.fromList (ctor1:ctors))
        pure (cType :@ Just span')