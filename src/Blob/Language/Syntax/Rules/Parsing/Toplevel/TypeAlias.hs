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

module Blob.Language.Syntax.Rules.Parsing.Toplevel.TypeAlias where

import Blob.Language.Syntax.Parser (Parser)
import Blob.Language.Syntax.Internal.Parsing.Located
import Blob.Language.Syntax.Internal.Parsing.AST
import Blob.Language.Syntax.Internal.Parsing.Helpers
import Blob.Language.Syntax.Rules.Parsing.Keyword
import Blob.Language.Syntax.Rules.Parsing.Identifier
import Blob.Language.Syntax.Rules.Parsing.Symbol
import Blob.Language.Syntax.Rules.Parsing.Types.Arrow
import Control.Applicative (many)

typeAlias :: Parser (Located Statement)
typeAlias = do
    (span', alias) <- getPositionInSource $ do
        iPos <- getPositionAndIndent
        keyword "type"
        name <- sameLineOrIndented iPos typeIdentifier
        tvs <- many $ sameLineOrIndented iPos identifier
        sameLineOrIndented iPos (symbol "=")
        (span', alias) <- getPositionInSource $ sameLineOrIndented iPos type'

        pure (TypeDeclaration name tvs (TAlias alias :@ Just span'))

    pure (alias :@ Just span')