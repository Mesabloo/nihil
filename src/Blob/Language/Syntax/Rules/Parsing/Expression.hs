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

module Blob.Language.Syntax.Rules.Parsing.Expression where

import Blob.Language.Syntax.Parser (Parser)
import Blob.Language.Syntax.Internal.Parsing.Located
import Blob.Language.Syntax.Internal.Parsing.AST
import Blob.Language.Syntax.Internal.Parsing.Helpers
import Blob.Language.Syntax.Internal.Lexing.SourceSpan (SourceSpan(..), begin, end)
import Blob.Language.Syntax.Rules.Parsing.Symbol
import Blob.Language.Syntax.Rules.Parsing.Types.Arrow
import {-# SOURCE #-} Blob.Language.Syntax.Rules.Parsing.Expressions.Atom
import Blob.Language.Syntax.Rules.Parsing.Expressions.Where
import Text.Megaparsec ((<?>), some, optional, try, (<|>))
import Control.Lens

expression :: Parser (Located Expr)
expression = flip (<?>) "expression" $ do
    iPos <- getPositionAndIndent
    (span', (a, ty)) <- getPositionInSource $ do
        as <- some (sameLineOrIndented iPos atom)
        optional (sameLineOrIndented iPos (symbol "::" <|> symbol "∷") *> sameLineOrIndented iPos type') <&> (as,)
    w <- optional . try . getPositionInSource . sameLineOrIndented iPos $
        where'

    let expr = case ty of
            Nothing -> a :@ Just span'
            Just t  -> [AAnn (a :@ Just span') t :@ Just span'] :@ Just span'
    case w of
        Nothing -> pure expr
        Just (span'', ss) -> pure $ [AWhere expr ss :@ Just (SourceSpan (span' ^. begin) (span'' ^. end))]
                                        :@ Just (SourceSpan (span' ^. begin) (span'' ^. end))
