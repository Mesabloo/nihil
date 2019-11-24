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

module Blob.Interactive.Commands.Bench where

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
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.Text as Text
import Control.Lens ((^.))
import Control.Monad.State (get, liftIO)
import Text.PrettyPrint.ANSI.Leijen (text, yellow, integer, linebreak)
import Criterion.Measurement (secs)
import Control.Monad (replicateM)

-- | The 'Bench' command parser.
--
-- @:bench@
bench :: CommandParser Command
bench = do
    C.space *> (try . hidden) (keyword "bench") <* C.space <?> "߷"

    observing (lookAhead eof) >>= \case
        Right _ -> fail "Missing arguments \"[n] [expr]\""
        Left _  -> do
            n    <- L.decimal
            end' <- observing . lookAhead $ eof
            case end' of
                Right _ -> fail "Missing argument \"[expr]\""
                Left _  -> Bench n <$> (anySingle `someTill` eof)

execBench :: Integer -> String -> REPL ()
execBench n expr = do
    st <- get

    tks <- rethrowEither (text . errorBundlePretty) $ runLexer (Text.pack expr) "interactive"

    e <- rethrowEither printParseError $ runParser' (try expression <* eof) tks "interactive"

    (e, _) <- rethrowEither id $ runSugar
        ( do { accumulateOnExpression e
             ; desugarExpression "interactive" e } ) (st ^. op)

    rethrowEither id $ inferExpr (st ^. ctx) e

    t' <- liftIO . replicateM (fromIntegral n) . time $ runEval' e (st ^. values)

    let t   = map (uncurry const) t'
        min = minimum t
        max = maximum t
        avg = uncurry (/) . foldr (\e (s, c) -> (e + s, c + 1)) (0.0, 0.0) $ t
        stddev =
            let sigma = sum $ ((^ (2 :: Integer)) . subtract avg) <$> t
            in sqrt (sigma / fromIntegral n)

    liftIO $
        print . yellow $
            text "Results for " <> integer n <> text " runs:" <> linebreak <>
            text "> Minimum: " <> text (secs min) <> linebreak <>
            text "> Maximum: " <> text (secs max) <> linebreak <>
            text "> Average: " <> text (secs avg) <> linebreak <>
            text "> Std dev: " <> text (secs stddev) <> linebreak