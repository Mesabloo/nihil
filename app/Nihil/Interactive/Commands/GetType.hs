-- The Great Nihil Compiler
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

module Nihil.Interactive.Commands.GetType where

import Nihil.Interactive.Command (CommandParser, keyword, Command(..))
import Nihil.Interactive.REPL (REPL, ctx, op)
import Nihil.Interactive.Commands.Common
import Language.Nihil (runLexer, runParser', runSugar)
import Text.PrettyPrint.ANSI.Leijen (pretty, text, cyan, yellow)
import Language.Nihil.PrettyPrinting.CoreAST ()
import Language.Nihil.TypeChecking.Internal.Type (Scheme(..))
import Language.Nihil.Syntax.Rules.Parsing.Expression (expression)
import Language.Nihil.Syntax.Internal.Desugaring.Accumulator.Expression (accumulateOnExpression)
import Language.Nihil.Syntax.Rules.Desugaring.Expression (desugarExpression)
import Text.Megaparsec (try, hidden, (<|>), (<?>), observing, lookAhead, eof, anySingle, someTill, errorBundlePretty)
import qualified Text.Megaparsec.Char as C
import Control.Lens ((^.))
import Control.Monad.State (get, liftIO)
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

    liftIO $ do
        putStr (show . yellow $ pretty e)
        putStr " :: "
        print (cyan $ pretty type')