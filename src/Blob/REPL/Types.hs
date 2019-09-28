-- | This module holds the types for the REPL.
module Blob.REPL.Types where

import Blob.Language.Desugaring.Types (SugarState)
import Blob.Language.TypeChecking.Types (GlobalEnv)
import Text.PrettyPrint.Leijen (Doc)
import Control.Monad.State (StateT)
import Control.Monad.Except (ExceptT)
import System.Console.Haskeline (InputT)
import Blob.Interpreter.Types
import Data.Void
import Text.Megaparsec (Parsec)

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
    = REPLState { ctx :: GlobalEnv        -- ^ The 'GlobalEnv' used for running the type inference process
                , values :: EvalState     -- ^ The 'EvalState' used for running the interpreter
                , op :: SugarState        -- ^ The 'SugarState' used for running the desugaring process
                , prompt :: String        -- ^ The prompt symbol (defaults to @"> "@)
                , preload :: [FilePath]   -- ^ The files to preload
                }

-- | The 'REPL' monad.
type REPL = InputT (StateT REPLState (ExceptT REPLError IO))

-- | REPL errors type alias.
type REPLError = Doc

-- | The REPL startup options.
data REPLOptions
    = REPLOptions { preloadFiles :: [FilePath]  -- ^ Files to preload
                  }