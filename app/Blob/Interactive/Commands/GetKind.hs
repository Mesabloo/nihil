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

module Blob.Interactive.Commands.GetKind where

import Blob.Interactive.Command (CommandParser, keyword, Command(..))
import Blob.Interactive.Commands.Common
import Blob.Interactive.REPL (REPL, ctx, op)
import Blob.Language (runLexer, runParser', runSugar, runKI)
import Blob.Language.TypeChecking.Rules.Kinds.Infer (infer)
import Text.PrettyPrint.Leijen (pretty)
import Blob.Language.PrettyPrinting.Kinds ()
import Blob.Language.PrettyPrinting.Types ()
import Blob.Language.Syntax.Rules.Parsing.Types.Arrow (type')
import Blob.Language.TypeChecking.Internal.Environment (typeDeclCtx)
import Blob.Language.Syntax.Rules.Desugaring.Types.Type (desugarType)
import Blob.Language.TypeChecking.Rules.Types.Infer (tiType)
import System.Console.ANSI
import Text.Megaparsec (try, hidden, (<|>), (<?>), anySingle, someTill, observing, lookAhead, eof, errorBundlePretty)
import qualified Text.Megaparsec.Char as C
import System.IO (hFlush, stdout)
import Control.Monad.State (get, liftIO)
import Text.PrettyPrint.Leijen (text)
import qualified Data.Text as Text
import Control.Lens ((^.))

-- | The 'GetKind' command parser.
--
-- Either @:k@ or @:kind@.
getKind :: CommandParser Command
getKind = do
    C.space *> (try . hidden) (keyword "kind" <|> keyword "k") <* C.space <?> "߷"

    observing (lookAhead eof) >>= \case
        Right _ -> fail "Missing argument \"[type]\""
        Left _  -> GetKind <$> (anySingle `someTill` eof)

getKind' :: String -> REPL ()
getKind' typeExpr = do
    st <- get

    tks <- rethrowEither (text . errorBundlePretty) $ runLexer (Text.pack typeExpr) "interactive"

    t <- rethrowEither printParseError $ runParser' (type' <* eof) tks "interactive"

    (t, _) <- rethrowEither id $ runSugar (desugarType "interactive" t) (st ^. op)

    let t1 = tiType t
    (kind, _) <- rethrowEither id $ runKI (st ^. ctx . typeDeclCtx) (infer t1)

    liftIO $ setSGR [SetColor Foreground Vivid Yellow]
        >> putStr (show (pretty t1))
        >> setSGR [Reset]
        >> putStr " :: "
        >> setSGR [SetColor Foreground Vivid Cyan]
        >> print (pretty kind)
        >> setSGR [Reset]
        >> hFlush stdout