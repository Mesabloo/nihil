{-# LANGUAGE TemplateHaskell, FlexibleInstances, MultiParamTypeClasses #-}

-- | This module holds the types for the REPL.
module Blob.REPL.Types where

import Blob.Language.Desugaring.Types (SugarState)
import Blob.Language.TypeChecking.Types (GlobalEnv)
import Text.PrettyPrint.Leijen (Doc)
import Control.Monad.State (StateT(..), lift, MonadState(..))
import Control.Monad.Except (ExceptT(..), MonadError(..), runExceptT)
import System.Console.Haskeline (InputT, MonadException(..), RunIO(..), runInputT, defaultSettings)
import Blob.Interpreter.Types
import Data.Void
import Text.Megaparsec (Parsec)
import Control.Lens


-- | The command line parser.
type Parser = Parsec Void String

data Command = GetType String
             | GetKind String
             | Help
             | Code String
             | Load String
             | Exit
             | ResetEnv [String]
             | Time String
             | Bench Integer String
             | Env
    deriving (Eq, Ord, Show)

-- | The state used in the 'REPL'.
data REPLState
    = REPLState { _ctx :: GlobalEnv        -- ^ The 'GlobalEnv' used for running the type inference process
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
