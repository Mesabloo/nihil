module Blob.REPL.Types where

import Blob.Desugaring.Types (Expr, SugarState)
import Blob.TypeChecking.Types (GlobalEnv)
import Blob.Pretty.Parser (pExpression)
import Text.PrettyPrint.Leijen (Doc)
import qualified Data.Map as Map
import Control.Monad.State (StateT)
import Control.Monad.Reader (ReaderT)
import Control.Monad.Except (Except, ExceptT)
import System.Console.Haskeline (InputT)
import Data.List (intercalate)
import Blob.Parsing.Annotation
import Blob.Interpreter.Types

data Command = GetType String
             | GetKind String
             | Help
             | Code String
             | Load String
             | Exit
             | Reload
             | Ast String
             | Time String
             | Bench Integer String
             | Env
    deriving (Eq, Ord, Show)



data REPLState = REPLState { ctx :: GlobalEnv
                           , values :: EvalState
                           , lastExecTime :: Double
                           , op :: SugarState }

type REPL = InputT (StateT REPLState (ExceptT REPLError IO))

type REPLError = Doc

data REPLOptions = REPLOptions { preload :: [FilePath] }