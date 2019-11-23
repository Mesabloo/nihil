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

module Blob.Language.TypeChecking.Internal.Errors.InfiniteKind where

import Blob.Language.TypeChecking.Internal.Kind (Kind)
import Blob.Language.TypeChecking.KindChecker (KIError)
import Blob.Language.PrettyPrinting.Kinds ()
import Text.PrettyPrint.Leijen

makeInfiniteKindError :: String -> Kind -> KIError
makeInfiniteKindError s k1 =
    text "Occur check fails: cannot construct the infinite kind " <> text s <> text " ~ " <> pretty k1 <> dot <> linebreak
