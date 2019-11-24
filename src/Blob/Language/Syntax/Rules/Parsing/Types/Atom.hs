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

module Blob.Language.Syntax.Rules.Parsing.Types.Atom where

import Blob.Language.Syntax.Internal.Parsing.AST
import Blob.Language.Syntax.Internal.Parsing.Helpers
import Blob.Language.Syntax.Internal.Parsing.Located
import Blob.Language.Syntax.Parser (Parser)
import Blob.Language.Syntax.Rules.Parsing.Identifier
import Blob.Language.Syntax.Rules.Parsing.Types.Tuple
import Blob.Language.Syntax.Rules.Parsing.Types.List
import Blob.Language.Syntax.Rules.Parsing.Types.Identifier
import {-# SOURCE #-} Blob.Language.Syntax.Rules.Parsing.Types.Arrow
import Text.Megaparsec (choice, try)
import Control.Lens ((^.))

atype :: Parser (Located Type)
atype = do
    (span', at) <- getPositionInSource $
        choice [ conid, try tTuple, tList, TVar <$> identifier, (^. located) <$> parens type' ]
    pure (at :@ Just span')