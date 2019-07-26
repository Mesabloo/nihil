module Blob.Language.Parsing.Types where

import qualified Text.Megaparsec as Mega
import Data.Void
import Data.Text
import qualified Data.Map as Map
import Blob.Language.Parsing.Annotation

type Parser = Mega.Parsec Void Text

---------------------------------------------------------------------------
---------------------------------------------------------------------------
---------------------------------------------------------------------------

type Program = [Annotated Statement]

data Statement
    = Declaration String (Annotated Type)
    | Definition String [String] (Annotated Expr)
    | OpFixity String (Annotated Fixity)
    | TypeDeclaration String [String] (Annotated CustomType)
    | Empty
    deriving (Show, Eq, Ord)

type Expr = [Annotated Atom]

data Atom
    = ALit Literal
    | AId String
    | AOperator String
    | AList [Annotated Expr]
    | ATuple [Annotated Expr]
    | AHole
    | ALambda [String] (Annotated Expr)
    | AMatch (Annotated Expr) [([Annotated Pattern], Annotated Expr)]
    | AParens (Annotated Expr)
    | AApp (Annotated Atom) (Annotated Atom)
    | AAnn (Annotated Expr) (Annotated Type)
    deriving (Show, Ord, Eq)

data Pattern
    = PId String
    | PLit Literal
    | PCtor String [[Annotated Pattern]]
    | PTuple [[Annotated Pattern]]
    | PList [[Annotated Pattern]]
    | PHole
    | PParens [Annotated Pattern]
    | POperator String
    | PAnn [Annotated Pattern] (Annotated Type)
    deriving (Show, Ord, Eq)

data Literal
    = LInt Integer
    | LChr Char
    | LDec Double
    | LStr String
    deriving (Show, Ord, Eq)

data Fixity
    = Infix Associativity Integer String
    deriving (Show, Eq, Ord)

data Associativity = L | R | N
    deriving (Show, Eq, Ord)

data Type
    = TId String            -- Type
    | TTuple [Annotated Type]         -- (a, b, ...)
    | TList [Annotated Type]          -- [a, b, ...]
    | TArrow (Annotated Expr) (Annotated Type) (Annotated Type) -- a ->{n} b ...
    | TFun (Annotated Type) (Annotated Type)        -- a -o b ...
    | TVar String           -- a...
    | TApp [Annotated Type]        -- Type a...
    deriving (Show, Eq, Ord)

data CustomType = TSum (Map.Map String [Annotated Type]) | TAlias (Annotated Type)
    deriving (Show, Eq, Ord)