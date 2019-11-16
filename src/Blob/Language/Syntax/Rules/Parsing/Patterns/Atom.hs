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

module Blob.Language.Syntax.Rules.Parsing.Patterns.Atom where

import Blob.Language.Syntax.Internal.Parsing.Helpers
import Blob.Language.Syntax.Internal.Parsing.AST
import Blob.Language.Syntax.Internal.Parsing.Located
import Blob.Language.Syntax.Parser (Parser)
import Blob.Language.Syntax.Rules.Parsing.Literal
import Blob.Language.Syntax.Rules.Parsing.Identifier
import Blob.Language.Syntax.Rules.Parsing.Symbol
import {-# SOURCE #-} Blob.Language.Syntax.Rules.Parsing.Pattern
import Blob.Language.Syntax.Rules.Parsing.Types.Arrow
import Blob.Language.Syntax.Rules.Parsing.Wildcard
import Blob.Language.Syntax.Rules.Parsing.Patterns.List
import Blob.Language.Syntax.Rules.Parsing.Patterns.Tuple
import Text.Megaparsec (choice, try, (<|>), many, optional)

patTerm :: Parser (Located Pattern)
patTerm = do
    iPos <- getPositionAndIndent
    (span', (term, ty)) <- getPositionInSource $ do
        p <- choice [ PHole       <$ hole
                    , PLit . LDec <$> try float
                    , PLit . LInt <$> integer
                    , PLit . LChr <$> char
                    , PId         <$> identifier
                    , PCtor       <$> typeIdentifier <*> (fmap (: []) <$> many (sameLineOrIndented iPos patTerm))
                    ,                 try patTuple
                    ,                 patList
                    , PLit . LStr <$> string
                    , PParens     <$> parens pattern' ]
        (p,) <$> optional (sameLineOrIndented iPos (symbol "::" <|> symbol "∷") *> sameLineOrIndented iPos type')

    case ty of
        Nothing -> pure (term :@ Just span')
        Just t  -> pure (PAnn [term :@ Just span'] t :@ Just span')