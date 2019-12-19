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

module Nihil.Interactive.Commands.Code where

import Nihil.Interactive.Command (CommandParser, Command(..))
import Nihil.Interactive.Commands.Common
import Nihil.Interactive.REPL (REPL, ctx, op, values)
import Language.Nihil (Located(..), Program(..), Statement(..), Expr(EId, EApp), Pattern(PId), tiProgram, runCheck, runDesugarer, runSugar, runParser', runLexer)
import Nihil.Interpreter.Scope (_Scope)
import Nihil.Interpreter.Evaluator (vals)
import Nihil.Interpreter (runEval')
import Nihil.Interpreter.Value (Value(VLam))
import Language.Nihil.Syntax.Rules.Parsing.Expression (expression)
import Language.Nihil.Syntax.Rules.Parsing.Program (program)
import Language.Nihil.Syntax.Internal.Desugaring.Accumulator.Expression (accumulateOnExpression)
import Language.Nihil.Syntax.Rules.Desugaring.Expression (desugarExpression)
import Text.Megaparsec (eof, anySingle, someTill, (<|>), try, eitherP, errorBundlePretty)
import Control.Lens ((^.), (.=), (%=), (%~))
import Control.Monad.State (get, liftIO)
import Control.Monad.Except (throwError)
import qualified Data.Map as Map
import qualified Data.Text as Text
import Text.PrettyPrint.ANSI.Leijen (text, pretty, cyan)
import Control.Monad (forM_)

-- | When the entire input is some 'Code'.
code :: CommandParser Command
code = (eof *> fail "") <|> Code <$> anySingle `someTill` eof

execCode :: String -> REPL ()
execCode stat = do
    st <- get
    x <- rethrowEither (text . errorBundlePretty) $ runLexer (Text.pack stat) "interactive"

    rethrowEither printParseError (runParser' (eitherP (try program) expression <* eof) x "interactive")
     >>= \case
        Right e -> do
            (e, _) <- rethrowEither id $ runSugar
                ( do { accumulateOnExpression e
                     ; desugarExpression "interactive" e } ) (st ^. op)

            rethrowEither id $ inferExpr (st ^. ctx) e

            liftIO (runEval' e (st ^. values))
            >>= \case
                Left err -> throwError err
                Right evalRes ->
                    liftIO $ print (cyan $ pretty evalRes)

        Left p  -> do
            (p'@(Program ss :@ _), state') <- rethrowEither id $ runSugar (runDesugarer "interactive" (p :@ Nothing)) (st ^. op)

            op .= state'
            (_, state') <- rethrowEither id $ runCheck (st ^. ctx) (tiProgram p')
            ctx .= state'

            forM_ ss $ \case
                Definition id' expr :@ _ -> do
                    st'   <- get
                    liftIO (runEval' expr ((vals . _Scope %~ Map.insert id' (lambda id' (st' ^. values . vals))) (st' ^. values)))
                    >>= \case
                        Left err -> throwError err
                        Right evalRes ->
                            values . vals . _Scope %= Map.insert id' evalRes
                _                   -> pure ()
          where lambda fun = VLam (PId "x" :@ Nothing) (EApp (EId fun :@ Nothing) (EId "x" :@ Nothing) :@ Nothing)