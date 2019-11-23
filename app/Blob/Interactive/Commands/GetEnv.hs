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

module Blob.Interactive.Commands.GetEnv where

import Blob.Interactive.Command (CommandParser, keyword, Command(..))
import Blob.Interactive.REPL (REPL, ctx)
import Blob.Language.TypeChecking.Internal.Environment (TypeEnv(..), KindEnv(..), defCtx, ctorCtx, typeDeclCtx)
import Blob.Language.TypeChecking.Internal.Type (Scheme(..))
import Text.PrettyPrint.ANSI.Leijen (pretty, text, cyan, yellow)
import Blob.Language.PrettyPrinting.Types ()
import Blob.Language.PrettyPrinting.Kinds ()
import Text.Megaparsec (try, hidden, (<?>))
import qualified Text.Megaparsec.Char as C
import Data.Functor (($>))
import qualified Data.Map as Map
import Control.Lens ((^.), use)
import Data.Function (on)
import Control.Monad (forM_)
import Control.Monad.State (liftIO)

-- | The 'Env' command parser.
--
-- @:env@
env :: CommandParser Command
env = C.space *> (try . hidden) (keyword "env") <* C.space $> Env <?> "߷"

getEnv :: REPL ()
getEnv = do
    st  <- use ctx
    let (KindEnv types) = st ^. typeDeclCtx
        types'          = Map.toList types
        (TypeEnv funs)  = (uncurry ((<>) `on` (st ^.)) (defCtx, ctorCtx))
        funs'           = Map.toList funs
        showKinds       = forM_ types' $
                \(name, kind) -> do
                    putStr "\t"
                    putStr (show . yellow $ text name)
                    putStr " :: "
                    print (cyan $ pretty kind)
        showFuns        = forM_ funs' $
                \(name, Scheme _ type') -> do
                    putStr "\t"
                    putStr (show . yellow $ text name)
                    putStr " :: "
                    print (cyan $ pretty type')

    liftIO $ putStrLn "Types:" *> showKinds *> putStrLn ""
    liftIO $ putStrLn "Functions:" *> showFuns