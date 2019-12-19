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

module Language.Nihil.Syntax.Internal.Desugaring.CoreAST where

import Language.Nihil.Syntax.Internal.Parsing.Located
import qualified Data.Map as Map

-- | The 'Expr' AST Node, either:
data Expr
    = EId String                                                      -- ^ An identifier
    | ELit Literal                                                    -- ^ A literal
    | ELam (Located Pattern) (Located Expr)                       -- ^ An anonymous function (lambda function)
    | EApp (Located Expr) (Located Expr)                          -- ^ An application
    | ETuple [Located Expr]                                         -- ^ A tuple
    | EMatch (Located Expr) [(Located Pattern, Located Expr)]   -- ^ A @match@ expression (pattern-matching)
    | EHole                                                           -- ^ A type hole
    | EAnn (Located Expr) (Located Type)                          -- ^ An annotated expression
    | ELet [Located Statement] (Located Expr)                     -- ^ A @let@ expression
    deriving (Show, Eq, Ord)

-- | The 'Pattern' AST Node, either:
data Pattern
    = Wildcard                                   -- ^ A wildcard
    | PId String                                 -- ^ An identifier
    | PInt Integer                               -- ^ An integer
    | PDec Double                                -- ^ A decimal number
    | PChr Char                                  -- ^ A character
    | PTuple [Located Pattern]                 -- ^ A tuple
    | PCtor String [Located Pattern]           -- ^ A data type constructor
    | PAnn (Located Pattern) (Located Type)  -- ^ A type-annotated pattern
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
newtype Program = Program [Located Statement]
    deriving (Eq, Ord, Show)

-- | Returns the 'Statement's wrapped inside a 'Program' node.
getStatements :: Program -> [Located Statement]
getStatements (Program s) = s

-- | The 'Statement' AST Node, which can be either:
data Statement
    = Declaration String (Located Type)                      -- ^ A function declaration
    | Definition String (Located Expr)                       -- ^ A function definition
    | TypeDeclaration String [String] (Located CustomType)   -- ^ A custom type declaration
    deriving (Eq, Ord, Show)

---------------------------------------------------------------------------------------------
{- Types -}

-- | The 'Scheme' Node used to contain a type and its type variables.
data Scheme = Scheme [String] (Located Type)
    deriving (Eq, Ord, Show)

-- | The 'Type' AST Node, either:
data Type
    = TId String                              -- ^ An identifier
    | TTuple [Located Type]                 -- ^ A tuple
    | TFun (Located Type, Integer) (Located Type)  -- ^ A linear function
    | TVar String                             -- ^ A free type variable (unused node in desugaring)
    | TApp (Located Type) (Located Type)  -- ^ A type application
    deriving (Eq, Ord, Show)

-- | The 'CustomType' AST Node, either:
data CustomType
    = TSum (Map.Map String Scheme)  -- ^ A sum type
    | TAlias (Located Type)       -- ^ A type alias
    deriving (Eq, Ord, Show)
