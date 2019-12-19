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

{-# LANGUAGE LambdaCase #-}

module Nihil.Interactive.Commands.GetKind where

import Nihil.Interactive.Command (CommandParser, keyword, Command(..))
import Nihil.Interactive.Commands.Common
import Nihil.Interactive.REPL (REPL, ctx, op)
import Language.Nihil (runLexer, runParser', runSugar, runKI)
import Language.Nihil.TypeChecking.Rules.Kinds.Infer (infer)
import Text.PrettyPrint.ANSI.Leijen (pretty, text, yellow, cyan)
import Language.Nihil.PrettyPrinting.Kinds ()
import Language.Nihil.PrettyPrinting.Types ()
import Language.Nihil.Syntax.Rules.Parsing.Types.Arrow (type')
import Language.Nihil.TypeChecking.Internal.Environment (typeDeclCtx)
import Language.Nihil.Syntax.Rules.Desugaring.Types.Type (desugarType)
import Language.Nihil.TypeChecking.Rules.Types.Infer (tiType)
import Text.Megaparsec (try, hidden, (<|>), (<?>), anySingle, someTill, observing, lookAhead, eof, errorBundlePretty)
import qualified Text.Megaparsec.Char as C
import Control.Monad.State (get, liftIO)
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
    kind <- rethrowEither id $ runKI (st ^. ctx . typeDeclCtx) (infer t1)

    liftIO $ do
        putStr (show . yellow $ pretty t1)
        putStr " :: "
        print (cyan $ pretty kind)