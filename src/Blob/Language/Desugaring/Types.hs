-- Blobc, a compiler for compiling Blob source code
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

{-# LANGUAGE FlexibleInstances, TypeFamilies, RecordWildCards, TemplateHaskell #-}

-- | This modules holds the types for the desugaring process.
module Blob.Language.Desugaring.Types where

import qualified Data.Map as Map
import Control.Monad.State
import Control.Monad.Except
import qualified Blob.Language.Parsing.Types as P (Fixity)
import Text.PrettyPrint.Leijen (Doc)
import Blob.Language.Parsing.Annotation
import Control.Lens

-- | The 'Expr' AST Node, either:
data Expr
    = EId String                                                      -- ^ An identifier
    | ELit Literal                                                    -- ^ A literal
    | ELam (Annotated Pattern) (Annotated Expr)                       -- ^ An anonymous function (lambda function)
    | EApp (Annotated Expr) (Annotated Expr)                          -- ^ An application
    | ETuple [Annotated Expr]                                         -- ^ A tuple
    | EMatch (Annotated Expr) [(Annotated Pattern, Annotated Expr)]   -- ^ A @match@ expression (pattern-matching)
    | EHole                                                           -- ^ A type hole
    | EAnn (Annotated Expr) (Annotated Type)                          -- ^ An annotated expression
    | ELet [Annotated Statement] (Annotated Expr)                     -- ^ A @let@ expression
    deriving (Show, Eq, Ord)

-- | The 'Pattern' AST Node, either:
data Pattern
    = Wildcard                                   -- ^ A wildcard
    | PId String                                 -- ^ An identifier
    | PInt Integer                               -- ^ An integer
    | PDec Double                                -- ^ A decimal number
    | PChr Char                                  -- ^ A character
    | PTuple [Annotated Pattern]                 -- ^ A tuple
    | PCtor String [Annotated Pattern]           -- ^ A data type constructor
    | PAnn (Annotated Pattern) (Annotated Type)  -- ^ A type-annotated pattern
    deriving (Show, Eq, Ord)

-- | A literal, either:
data Literal
    = LInt Integer  -- ^ An integer
    | LDec Double   -- ^ A decimal number
    | LChr Char     -- ^ A character
    deriving (Show, Eq, Ord)

---------------------------------------------------------------------------------------------
{- AST -}

-- | The 'Program' AST Node, containing many 'Statement's.
newtype Program = Program [Annotated Statement]
    deriving (Eq, Ord, Show)

-- | Returns the 'Statement's wrapped inside a 'Program' node.
getStatements :: Program -> [Annotated Statement]
getStatements (Program s) = s

-- | The 'Statement' AST Node, which can be either:
data Statement
    = Declaration String (Annotated Type)                      -- ^ A function declaration
    | Definition String (Annotated Expr)                       -- ^ A function definition
    | TypeDeclaration String [String] (Annotated CustomType)   -- ^ A custom type declaration
    deriving (Eq, Ord, Show)

---------------------------------------------------------------------------------------------
{- Types -}

-- | The 'Scheme' Node used to contain a type and its type variables.
data Scheme = Scheme [String] (Annotated Type)
    deriving (Eq, Ord, Show)

-- | The 'Type' AST Node, either:
data Type
    = TId String                              -- ^ An identifier
    | TTuple [Annotated Type]                 -- ^ A tuple
    | TFun (Annotated Type, Integer) (Annotated Type)  -- ^ A linear function
    | TRVar String                            -- ^ A rigid type variable
    | TVar String                             -- ^ A free type variable (unused node in desugaring)
    | TApp (Annotated Type) (Annotated Type)  -- ^ A type application
    deriving (Eq, Ord, Show)

-- | The 'CustomType' AST Node, either:
data CustomType
    = TSum (Map.Map String Scheme)  -- ^ A sum type
    | TAlias (Annotated Type)       -- ^ A type alias
    deriving (Eq, Ord, Show)

---------------------------------------------------------------------------------------------

-- | The Desugarer monad.
type Sugar = StateT SugarState (Except Doc)

-- | The state used in the 'Sugar' monad.
newtype SugarState
    = SugarState { _fixities :: Map.Map String P.Fixity -- ^ Contains all the declared operator fixities
                 }
makeLenses ''SugarState