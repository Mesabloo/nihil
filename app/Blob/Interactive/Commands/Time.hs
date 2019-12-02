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

module Blob.Interactive.Commands.Time where

import Blob.Interactive.Command (CommandParser, keyword, Command(..))
import Blob.Interactive.Commands.Common as Common
import Blob.Interactive.REPL (REPL, op, ctx, values)
import Blob.Interpreter (runEval')
import Blob.Language (runLexer, runParser', runSugar)
import Blob.Language.Syntax.Rules.Parsing.Expression (expression)
import Blob.Language.Syntax.Internal.Desugaring.Accumulator.Expression (accumulateOnExpression)
import Blob.Language.Syntax.Rules.Desugaring.Expression (desugarExpression)
import Text.Megaparsec (try, hidden, (<?>), observing, lookAhead, eof, anySingle, someTill, errorBundlePretty)
import qualified Text.Megaparsec.Char as C
import qualified Data.Text as Text
import Control.Lens ((^.))
import Control.Monad.State (get, liftIO)
import Control.Monad.Except (throwError)
import Text.PrettyPrint.ANSI.Leijen (text, pretty, yellow, cyan)
import Criterion.Measurement (secs)

-- | The 'Time' command parser.
--
-- @:time@
time :: CommandParser Command
time = do
    C.space *> (try . hidden) (keyword "time") <* C.space <?> "߷"

    observing (lookAhead eof) >>= \case
        Right _ -> fail "Missing argument \"[expr]\""
        Left _  -> Time <$> (anySingle `someTill` eof)

execTime :: String -> REPL ()
execTime expr = do
    st <- get
    tks <- rethrowEither (text . errorBundlePretty) $ runLexer (Text.pack expr) "interactive"

    e <- rethrowEither printParseError $ runParser' (try expression <* eof) tks "interactive"

    (e, _) <- rethrowEither id $ runSugar
        ( do { accumulateOnExpression e
             ; desugarExpression "interactive" e } ) (st ^. op)

    rethrowEither id $ inferExpr (st ^. ctx) e

    (t, res) <- liftIO . Common.time $ runEval' e (st ^. values)
    case res of
        Left err      -> throwError err
        Right evalRes ->
            liftIO $ print (cyan $ pretty evalRes)

    liftIO $ print (yellow $ text "Time taken: " <> text (secs t))