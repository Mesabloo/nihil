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

{-# LANGUAGE LambdaCase, TypeApplications #-}

module Blob.Interactive.Commands.GetType where

import Blob.Interactive.Command (CommandParser, keyword, Command(..))
import Blob.Interactive.REPL (REPL, ctx, op)
import Blob.Interactive.Commands.Common
import Blob.Language (runLexer, runParser', runSugar, Located, Expr)
import Blob.Language.PrettyPrinting.Pretty (pretty)
import Blob.Language.PrettyPrinting.CoreAST ()
import Blob.Language.TypeChecking.Internal.Type (Scheme(..), TVar)
import Blob.Language.Syntax.Rules.Parsing.Expression (expression)
import Blob.Language.Syntax.Internal.Desugaring.Accumulator.Expression (accumulateOnExpression)
import Blob.Language.Syntax.Rules.Desugaring.Expression (desugarExpression)
import Text.Megaparsec (try, hidden, (<|>), (<?>), observing, lookAhead, eof, anySingle, someTill, errorBundlePretty)
import qualified Text.Megaparsec.Char as C
import Control.Lens ((^.))
import Control.Monad.State (get, liftIO)
import Text.PrettyPrint.Leijen (text)
import System.IO (hFlush, stdout)
import System.Console.ANSI
import qualified Data.Text as Text

-- | The 'GetType' command parser.
--
-- Either @:type@ or @:t@.
getType :: CommandParser Command
getType = do
    C.space *> (try . hidden) (keyword "type" <|> keyword "t") <* C.space <?> "߷"

    observing (lookAhead eof) >>= \case
        Right _ -> fail "Missing argument \"[expr]\""
        Left _  -> GetType <$> (anySingle `someTill` eof)

getType' :: String -> REPL ()
getType' expr = do
    st <- get
    tks <- rethrowEither (text . errorBundlePretty) $ runLexer (Text.pack expr) "interactive"

    e <- rethrowEither printParseError $ runParser' (expression <* eof) tks "interactive"

    (e, _) <- rethrowEither id $ runSugar
        (do { accumulateOnExpression e
            ; desugarExpression "interactive" e } ) (st ^. op)

    (Scheme _ type') <- rethrowEither id $ inferExpr (st ^. ctx) e

    liftIO $ setSGR [SetColor Foreground Vivid Yellow]
        >> putStr (show (pretty e))
        >> setSGR [Reset]
        >> putStr " :: "
        >> setSGR [SetColor Foreground Vivid Cyan]
        >> print (pretty type')
        >> setSGR [Reset]
        >> hFlush stdout