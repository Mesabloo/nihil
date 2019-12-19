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

{-# LANGUAGE TypeApplications #-}

module Nihil.Interactive.Commands.Common where

import Nihil.Interactive.REPL (REPLError, REPL)
import Language.Nihil (Located, Expr)
import Language.Nihil.TypeChecking.Internal.Type (Scheme)
import Language.Nihil.TypeChecking.Internal.Environment (GlobalEnv)
import Language.Nihil.TypeChecking.TypeChecker (TIError)
import Language.Nihil.TypeChecking.Solver.TypeHoleSolver (runHoleInspect)
import Language.Nihil.TypeChecking.Solver.TypeSolver (runTypeSolver)
import Language.Nihil.TypeChecking.Rules.Types.Infer (runTI, infer)
import Language.Nihil.TypeChecking.Internal.Substitution (apply)
import Language.Nihil.TypeChecking.Internal.Substitution.Types (closeOver)
import Text.Megaparsec (ShowErrorComponent, ParseErrorBundle, parseErrorTextPretty, bundleErrors, Stream)
import Text.PrettyPrint.ANSI.Leijen (text, Doc)
import Data.Either (either)
import Control.Monad.Except (throwError)
import Criterion.Measurement (getTime)

printParseError :: (Stream s, ShowErrorComponent e) => ParseErrorBundle s e -> Doc
printParseError = text . concatMap parseErrorTextPretty . bundleErrors

-- | Rethrows the 'Either' computation, applying the function on the error, or returning the result.
rethrowEither :: (e -> REPLError) -> Either e a -> REPL a
rethrowEither f = either (throwError . f) pure

-- | A simple function to get the execution time of an action.
time :: IO a -> IO (Double, a)
time f = do
    begin  <- getTime
    result <- f
    end    <- getTime
    pure $ (,) (end - begin) result

-- | Solves the type for a given 'Expr' in a given 'GlobalEnv'.
inferExpr :: GlobalEnv -> Located Expr -> Either TIError Scheme
inferExpr env ex = do
    (ty, c) <- runTI env (infer ex)
    subst <- runTypeSolver env c
    runHoleInspect subst
    pure . closeOver $ apply subst ty