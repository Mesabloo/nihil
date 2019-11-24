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

{-# LANGUAGE LambdaCase #-}

module Blob.Interactive.Commands.Load where

import Blob.Interactive.Command (CommandParser, keyword, Command(..))
import Blob.Interactive.REPL (REPL, values, ctx, op)
import Blob.Interactive.Commands.Common
import Blob.Language (Program(..), Statement(..), Located(..), tiProgram, runSugar, runDesugarer, runParser, runLexer, runCheck, located)
import Blob.Interpreter (runEval')
import Blob.Interpreter.Evaluator (vals)
import Blob.Interpreter.Scope (_Scope)
import Text.Megaparsec (try, hidden, (<|>), (<?>), observing, lookAhead, eof, anySingle, someTill, errorBundlePretty)
import qualified Text.Megaparsec.Char as C
import Data.String.Utils (rstrip)
import Control.Monad.State (liftIO, get)
import Control.Monad.Except (throwError)
import Text.PrettyPrint.ANSI.Leijen (text, dot, linebreak)
import qualified Data.Text.IO as Text
import qualified Data.Map as Map
import Control.Lens ((^.), (%=), (.=))
import Control.Monad (forM_)
import System.Directory (doesFileExist)

-- | The 'Load' command parser.
--
-- Either @:load@ or @:l@.
load :: CommandParser Command
load = do
    C.space *> (try . hidden) (keyword "load" <|> keyword "l") <* C.space <?> "߷"

    observing (lookAhead eof) >>= \case
        Right _ -> fail "Missing argument \"[file]\""
        Left _  -> do
            file <- anySingle `someTill` eof
            pure . Load $ rstrip file

loadFile :: String -> REPL ()
loadFile file = do
    fileExists <- liftIO $ doesFileExist file

    if not fileExists
    then throwError (text "File \"" <> text file <> text "\" not found" <> dot <> linebreak)
    else do
        content       <- liftIO $ Text.readFile file
        st''          <- get
        tks           <- rethrowEither (text . errorBundlePretty) (runLexer content file)
        parseTree     <- rethrowEither printParseError (runParser tks file)

        let state' = st'' ^. op
        (ast, state') <- rethrowEither id $ runSugar (runDesugarer file (parseTree :@ Nothing)) state'

        op .= state'
        (_, state')   <- rethrowEither id $ runCheck (st'' ^. ctx) (tiProgram ast)
        ctx .= state'

        let (Program stmts) = ast ^. located

        forM_ stmts $ \case
            Definition id' expr :@ _ -> do
                st''    <- get
                liftIO $ runEval' expr (st'' ^. values)
                >>= \case
                    Left err      -> throwError err
                    Right evalRes -> values . vals . _Scope %= Map.insert id' evalRes
            _                   -> pure ()