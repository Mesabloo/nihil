module Blob.REPL.Types where

import Blob.Parsing.Types (ParseState, Expr)
import Blob.Inference.Types (GlobalEnv)
import Blob.PrettyPrinter.PrettyParser (pExpression)
import Text.PrettyPrint.Leijen (Doc)
import qualified Data.Map as Map
import Control.Monad.State (StateT)
import Control.Monad.Reader (ReaderT)
import Control.Monad.Except (Except, ExceptT)
import System.Console.Haskeline (InputT)
import Data.List (intercalate)

data Command = GetType String
             | GetKind String
             | Help
             | Code String
             | Load String
             | Exit
             | Reload
             | Eval String
             | Ast String
    deriving (Eq, Ord, Show)

data Value = VInt Integer
           | VStr String
           | VDec Double
           | VVar String
           | VLam String Expr (Scope Value)
           | VTuple [Value]
           | HLam (Value -> EvalEnv Value)
           | VList [Value]

data REPLState = REPLState { ctx :: GlobalEnv
                           , op :: ParseState
                           , values :: Map.Map String Value }

type REPL a = InputT (StateT REPLState (ExceptT REPLError IO)) a

type REPLError = Doc

type Scope = Map.Map String

type EvalEnv a = ReaderT (Map.Map String Value) (Except REPLError) a



instance Show Value where
    show (VInt i)     = show i
    show (VStr s)     = show s
    show (VDec d)     = show d
    show (VVar s)     = s
    show (VLam i e _) = "λ" ++ i ++ " → " ++ show (pExpression e 0)
    show (VTuple es)  = "(" ++ intercalate ", " (map show es) ++ ")"
    show (VList es)   = "[" ++ intercalate ", " (map show es) ++ "]"
    show (HLam _)     = "HLam _"

instance Eq Value where
    (==) (VInt i) (VInt i')          = i == i'
    (==) (VStr s) (VStr s')          = s == s'
    (==) (VDec d) (VDec d')          = d == d'
    (==) (VLam i e _) (VLam i' e' _) = i == i' && e == e'
    (==) (VTuple es) (VTuple es')    = es == es'
    (==) (VList es) (VList es')      = es == es'
    (==) _ _                         = False

instance Ord Value where
    (<=) (VInt i) (VInt i')          = i <= i'
    (<=) (VStr s) (VStr s')          = s <= s'
    (<=) (VDec d) (VDec d')          = d <= d'
    (<=) (VLam i e _) (VLam i' e' _) = i <= i' && e <= e'
    (<=) (VTuple es) (VTuple es')    = es <= es'
    (<=) (VList  es) (VList  es')    = es <= es'
    (<=) _ _                         = False
