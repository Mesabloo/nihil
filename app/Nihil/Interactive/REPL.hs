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

{-# LANGUAGE TemplateHaskell, FlexibleInstances, MultiParamTypeClasses #-}

module Nihil.Interactive.REPL where

import Language.Nihil.TypeChecking.Internal.Environment (GlobalEnv)
import Language.Nihil.Syntax.Desugarer (SugarState)
import Nihil.Interpreter.Evaluator (EvalState)
import Text.PrettyPrint.ANSI.Leijen (Doc)
import Control.Monad.State (StateT(..), lift, MonadState(..))
import Control.Monad.Except (ExceptT(..), MonadError(..), runExceptT)
import System.Console.Haskeline (InputT, MonadException(..), RunIO(..), runInputT, defaultSettings)
import Control.Lens (makeLenses)

-- | The state used in the 'REPL'.
data REPLState
    = REPLState
    { _ctx :: GlobalEnv        -- ^ The 'GlobalEnv' used for running the type inference process
    , _values :: EvalState     -- ^ The 'EvalState' used for running the interpreter
    , _op :: SugarState        -- ^ The 'SugarState' used for running the desugaring process
    , _prompt :: String        -- ^ The prompt symbol (defaults to @"> "@)
    , _preload :: [FilePath]   -- ^ The files to preload
    }
makeLenses ''REPLState

-- | The 'REPL' monad.
type REPL = InputT (StateT REPLState (ExceptT REPLError IO))

-- | REPL errors type alias.
type REPLError = Doc

-- | The REPL startup options.
data REPLOptions
    = REPLOptions { _preloadFiles :: [FilePath]  -- ^ Files to preload
                  }
makeLenses ''REPLOptions

------------------------------------------------------------------------

instance MonadException m => MonadException (ExceptT e m) where
    controlIO f = ExceptT $ controlIO $ \(RunIO run) ->
        let run' = RunIO (fmap ExceptT . run . runExceptT)
        in  runExceptT <$> f run'

instance MonadException m => MonadException (StateT s m) where
    controlIO f = StateT $ \s -> controlIO $ \(RunIO run) ->
        let run' = RunIO (fmap (StateT . const) . run . flip runStateT s)
        in  flip runStateT s <$> f run'

instance MonadError REPLError REPL where
    throwError = lift . lift . throwError
    m `catchError` h = do
        let s = runInputT defaultSettings m
        lift $ s `catchError` (runInputT defaultSettings . h)

instance MonadState REPLState REPL where
    get = lift get
    put = lift . put