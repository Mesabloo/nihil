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

module Language.Nihil.Syntax.Internal.Parsing.AST where

import qualified Data.Map as Map
import Language.Nihil.Syntax.Internal.Parsing.Located (Located)

-- | A simple program AST node containing many statement nodes.
type Program = [Located Statement]

-- | A simple statement AST node, which may be:
data Statement
    = Declaration String (Located Type)                     -- ^ A function declaration | @name :: type@
    | Definition String [Located Pattern] (Located Expr)  -- ^ A function definition | @name args = expression@
    | OpFixity String (Located Fixity)                      -- ^ An operator precedence declaration | @infixr 3 (++)@
    | TypeDeclaration String [String] (Located CustomType)  -- ^ A type declaration | @data X = Y@ or @type X = Y@
    | Empty                                                   -- ^ An unsupported kind of statement
    deriving (Show, Eq, Ord)

-- | A simple expression AST node containing many atom nodes.
type Expr = [Located Atom]

-- | A simple atom AST node, which can be:
data Atom
    = ALit Literal                                                     -- ^ A literal
    | AId String                                                       -- ^ An identifier
    | AOperator String                                                 -- ^ An operator
    | AList [Located Expr]                                           -- ^ A list
    | ATuple [Located Expr]                                          -- ^ A tuple
    | AHole                                                            -- ^ A type hole
    | ALambda [Located Pattern] (Located Expr)                     -- ^ An anonymous function (lambda function)
    | AMatch (Located Expr) [([Located Pattern], Located Expr)]  -- ^ A @match@ expression
    | AParens (Located Expr)                                         -- ^ A parenthesized expression
    | AApp (Located Atom) (Located Atom)                           -- ^ A function application
    | AAnn (Located Expr) (Located Type)                           -- ^ A type-annotated expression
    | ALet [Located Statement] (Located Expr)                      -- ^ A @let@ expression
    | AWhere (Located Expr) [Located Statement]                    -- ^ A @where@ expression
    deriving (Show, Ord, Eq)

-- | A simple pattern AST node, which might be:
data Pattern
    = PId String                                 -- ^ An identifier
    | PLit Literal                               -- ^ A literal
    | PCtor String [[Located Pattern]]         -- ^ A data type constructor
    | PTuple [[Located Pattern]]               -- ^ A tuple
    | PList [[Located Pattern]]                -- ^ A list
    | PHole                                      -- ^ A wildcard
    | PParens [Located Pattern]                -- ^ A parenthesized pattern
    | POperator String                           -- ^ An operator
    | PAnn [Located Pattern] (Located Type)  -- ^ A type-annotated pattern
    deriving (Show, Ord, Eq)

-- | A simple literal AST node, which may be:
data Literal
    = LInt Integer  -- ^ An integer
    | LChr Char     -- ^ A character
    | LDec Double   -- ^ A float
    | LStr String   -- ^ A string
    deriving (Show, Ord, Eq)

-- | A data type for holding the fixity of an operator
data Fixity
    = Infix Associativity Integer String
    deriving (Show, Eq, Ord)

-- | A data type showing the associativity of an operator (Left, Right or None)
data Associativity = L | R | N
    deriving (Show, Eq, Ord)

-- | A simple type AST node, which can be:
data Type
    = TId String                              -- ^ An identifier
    | TTuple [Located Type]                 -- ^ A tuple
    | TList [Located Type]                  -- ^ A list
    | TFun (Located Type, Integer) (Located Type)  -- ^ A linear function
    | TVar String                             -- ^ A rigid type variable
    | TApp [Located Type]                   -- ^ A type application
    deriving (Show, Eq, Ord)

-- | A custom type AST node, which might be:
data CustomType
    = TSum (Map.Map String [Located Type])   -- ^ A sum type
    | TAlias (Located Type)                  -- ^ A type alias
    | TGADT (Map.Map String (Located Type))  -- ^ A GADT (Generalized ADT)
    deriving (Show, Eq, Ord)
