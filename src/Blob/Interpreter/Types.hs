module Blob.Interpreter.Types where

import Blob.Language.Desugaring.Types
import Blob.Language.Parsing.Annotation
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except
import qualified Data.Map as Map
import Text.PrettyPrint.Leijen
import Blob.Language.Pretty.Parser (pExpression)
import Data.List (intercalate)

data Value = VInt Integer
           | VDec Double
           | VChr Char
           | VVar String
           | VLam String (Annotated Expr) (Scope Value)
           | VTuple [Value]
           | HLam (Value -> EvalEnv Value)
           | VCon String [Value]

type Scope = Map.Map String

type EvalEnv a = ReaderT EvalState (ExceptT EvalError IO) a

type EvalError = Doc

data EvalState = EvalState { vals :: Scope Value
                           , ctors :: [String] }




instance Show Value where
    show (VInt i)     = show i
    show (VDec d)     = show d
    show (VChr c)     = show c
    show (VVar s)     = s
    show (VLam i e _) = "(λ " <> i <> " → " <> show (pExpression e) <> ")"
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
