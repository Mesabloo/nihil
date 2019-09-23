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



data REPLState = REPLState { ctx :: GlobalEnv
                           , values :: EvalState
                           , op :: SugarState
                           , prompt :: String
                           , preload :: [FilePath] }

type REPL = InputT (StateT REPLState (ExceptT REPLError IO))

type REPLError = Doc

data REPLOptions = REPLOptions { preloadFiles :: [FilePath] }