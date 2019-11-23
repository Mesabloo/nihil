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

module Blob.Language
( Located.Located(..), Located.located
, CoreAST.Program(..), CoreAST.Statement(..), CoreAST.Expr(..), CoreAST.Pattern(..), CoreAST.Literal(..), CoreAST.Type(..)
, Syntax.runLexer, Syntax.runParser, Syntax.runParser', Syntax.runSugar, Syntax.runDesugarer
, Program.tiProgram, TypeChecking.runCheck, TypeChecking.runKI
, Pretty.Pretty, Pretty.pretty ) where

import qualified Blob.Language.Syntax.Internal.Parsing.Located as Located
import qualified Blob.Language.Syntax.Internal.Desugaring.CoreAST as CoreAST
import qualified Blob.Language.Syntax as Syntax
import qualified Blob.Language.TypeChecking as TypeChecking
import qualified Blob.Language.TypeChecking.Rules.Program as Program
import qualified Text.PrettyPrint.Leijen as Pretty