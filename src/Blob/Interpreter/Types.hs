-- The Blob programming language's interpreter.
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

{-# LANGUAGE TemplateHaskell #-}

-- | This module holds the types for the interpreter process.
module Blob.Interpreter.Types where

import Blob.Language.Desugaring.Types
import Blob.Language.Parsing.Annotation
import Control.Monad.Reader
import Control.Monad.Except
import qualified Data.Map as Map
import Text.PrettyPrint.Leijen
import Data.List (intercalate)
import Control.Lens

-- | The data type providing any sort of value, for example:
data Value
    = VInt Integer                                             -- ^ An integer
    | VDec Double                                              -- ^ A floating point number
    | VChr Char                                                -- ^ A character
    | VVar String                                              -- ^ A variable
    | VLam (Annotated Pattern) (Annotated Expr) (Scope Value)  -- ^ A lambda (remains unevaluated until the argument value is given)
    | VTuple [Value]                                           -- ^ A tuple
    | HLam (Value -> EvalEnv Value)                            -- ^ A native lambda (implemented in Haskell)
    | VCon String [Value]                                      -- ^ A data type constructor

-- | A type alias for convenient naming.
type Scope = Map.Map String

-- | The 'EvalEnv' monad used for evaluation.
type EvalEnv a = ReaderT EvalState (ExceptT EvalError IO) a

-- | The error type
type EvalError = Doc

-- | The state used in the 'EvalEnv' monad.
data EvalState
    = EvalState { _vals :: Scope Value  -- ^ A mapping of all the values of all the variables known
                , _ctors :: [String]    -- ^ All the data type constructors existing in the current session
                }
makeLenses ''EvalState



instance Show Value where
    show (VInt i)     = show i
    show (VDec d)     = show d
    show (VChr c)     = show c
    show (VVar s)     = s
    show (VTuple es)  = "(" <> intercalate ", " (map show es) <> ")"
    show (VCon id' e)
        | null e      = id'
        | otherwise   = "(" <> id' <> foldl (\acc t -> acc <> " " <> show t) "" e <> ")"
    show _            = ""

instance Eq Value where
    (==) (VInt i) (VInt i')          = i == i'
    (==) (VDec d) (VDec d')          = d == d'
    (==) (VChr v) (VChr v')          = v == v'
    (==) (VTuple es) (VTuple es')    = es == es'
    (==) (VCon id' e) (VCon id'' e') = id' == id'' && e == e'
    (==) _ _                         = False

instance Ord Value where
    (<=) (VInt i) (VInt i')          = i <= i'
    (<=) (VDec d) (VDec d')          = d <= d'
    (<=) (VChr v) (VChr v')          = v <= v'
    (<=) (VTuple es) (VTuple es')    = es <= es'
    (<=) (VCon id' _) (VCon id'' _)  = id' <= id''
    (<=) _ _                         = False
