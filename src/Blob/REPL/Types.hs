module Blob.REPL.Types where

import Blob.Parsing.Types (ParseState, Expr)
import Blob.TypeChecking.Types (GlobalEnv)
import Blob.Pretty.Parser (pExpression)
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
             | Ast String
             | Time String
             | Bench Integer String
             | Env
    deriving (Eq, Ord, Show)

data Value = VInt Integer
           | VDec Double
           | VChr Char
           | VVar String
           | VLam String Expr (Scope Value)
           | VTuple [Value]
           | HLam (Value -> EvalEnv Value)
           | VCon String [Value]

data REPLState = REPLState { ctx :: GlobalEnv
                           , op :: ParseState
                           , values :: EvalState
                           
                           , lastExecTime :: Double }

type REPL a = InputT (StateT REPLState (ExceptT REPLError IO)) a

type REPLError = Doc

type Scope = Map.Map String

type EvalEnv a = ReaderT EvalState (ExceptT REPLError IO) a

data EvalState = EvalState { vals :: Scope Value
                           , ctors :: [String] }




instance Show Value where
    show (VInt i)     = show i
    show (VDec d)     = show d
    show (VChr c)     = show c
    show (VVar s)     = s
    show (VLam i e _) = "(λ " <> i <> " → " <> show (pExpression e 0) <> ")"
    show (VTuple es)  = "(" <> intercalate ", " (map show es) <> ")"
    show (HLam _)     = "HLam _"
    show (VCon id' e) = "(" <> id' <> foldl (\acc t -> acc <> " " <> show t) "" e <> ")"

instance Eq Value where
    (==) (VInt i) (VInt i')          = i == i'
    (==) (VDec d) (VDec d')          = d == d'
    (==) (VChr v) (VChr v')          = v == v'
    (==) (VLam i e _) (VLam i' e' _) = i == i' && e == e'
    (==) (VTuple es) (VTuple es')    = es == es'
    (==) (VCon id' e) (VCon id'' e') = id' == id'' && e == e'
    (==) _ _                         = False

instance Ord Value where
    (<=) (VInt i) (VInt i')          = i <= i'
    (<=) (VDec d) (VDec d')          = d <= d'
    (<=) (VChr v) (VChr v')          = v <= v'
    (<=) (VLam i e _) (VLam i' e' _) = i <= i' && e <= e'
    (<=) (VTuple es) (VTuple es')    = es <= es'
    (<=) (VCon id' _) (VCon id'' _)  = id' <= id''
    (<=) _ _                         = False
