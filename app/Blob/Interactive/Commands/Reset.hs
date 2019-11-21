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

module Blob.Interactive.Commands.Reset where

import Blob.Interactive.Command (CommandParser, keyword, Command(..), opSymbol, identifier)
import Blob.Interpreter.Scope (_Scope)
import Blob.Interpreter.Evaluator (vals)
import Blob.Interactive.REPL (REPL, ctx, values)
import Blob.Language.TypeChecking.Internal.Environment (defCtx, _TypeEnv)
import Blob.Prelude (initGlobalEnv, initEvalState)
import Text.Megaparsec (try, hidden, (<|>), (<?>), many)
import qualified Text.Megaparsec.Char as C
import Control.Lens ((.=), (%=))
import qualified Data.Map as Map

-- | The 'ResetEnv' command parser.
--
-- Either @:r@ or @:reset@
reset :: CommandParser Command
reset = (C.space *> (try . hidden) (keyword "reset" <|> keyword "r") <* C.space <?> "߷")
     *> (ResetEnv <$> many (C.space *> (identifier <|> opSymbol) <* C.space))

resetEnv :: [String] -> REPL () -> REPL ()
resetEnv [] reload = do
    ctx .= initGlobalEnv
    values .= initEvalState

    reload
resetEnv [x] _ = resetOne x
resetEnv (x:xs) r = resetOne x *> resetEnv xs r

resetOne :: String -> REPL ()
resetOne x = do
    ctx . defCtx . _TypeEnv %= Map.delete x
    values . vals . _Scope %= Map.delete x