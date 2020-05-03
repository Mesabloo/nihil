{-| The concrete core representation of the language. It can be translated
    back to 'Nihil.Syntax.Concrete.Lexeme.Lexeme's and represents (without comments) what the user inputted.
-}

module Nihil.Syntax.Concrete.Core where

import qualified Data.Map as Map
import Nihil.Utils.Source (Located)

newtype Program = Program [AStatement]
  deriving
    ( -- | Use only for debugging
      Show
    , Eq)

type AStatement = Located Statement
data Statement
    = FunDeclaration String [AType]               -- ^ > { show :: Integer → String }
    | FunDefinition String [APattern] Expr        -- ^ > { fun x _ 4 = x - 9 }
    | OperatorFixity String AFixity               -- ^ > { infixl 4 + }
    | TypeDefinition String [String] ACustomType  -- ^ > { type Algebra f a = f a → a }
  deriving
    ( -- | Use only for debugging
      Show
    , Eq)

type AExpr = Located Expr
type Expr = [AAtom]
type AAtom = Located Atom
data Atom
    = ALiteral Literal                      -- ^ > { 9 or "hello" }
    | AId String                            -- ^ > { w }
    | AOperator String                      -- ^ > { >=> or `show` }
    | ATuple [Expr]                         -- ^ > { (e₁, e₂) }
    | ATypeHole                             -- ^ > { _ }
    | ALambda [APattern] Expr               -- ^ > { λ x y → e }
    | AMatch Expr [([APattern], Expr)]      -- ^ > { match e₁ with p → e₂ }
    | AParens Expr                          -- ^ > { (e) }
    | AApplication [AAtom]                  -- ^ > { f x }
    | ATypeAnnotated Expr [AType]           -- ^ > { e : t }
    | ALet [AStatement] Expr                -- ^ > { let x = y in e }
    | AWhere Expr [AStatement]              -- ^ > { f = g where g = e }
  deriving
    ( -- | Use only for debugging
      Show
    , Eq)

type APattern = Located Pattern
data Pattern
    = PId String                        -- ^ > { w }
    | PLiteral Literal                  -- ^ > { 2 or 'e' }
    | PConstructor String [APattern]    -- ^ > { Cons x xs }
    | PTuple [[APattern]]               -- ^ > { (p₁, p₂) }
    | PWildcard                         -- ^ > { _ }
    | PParens [APattern]                -- ^ > { (p) }
    | POperator String                  -- ^ > { `Cons` }
    | PTypeAnnotated [APattern] [AType] -- ^ > { p : t }
  deriving
    ( -- | Use only for debugging
      Show
    , Eq)

data Literal
    = LInteger Integer  -- ^ > { 0 or 125353 }
    | LCharacter Char   -- ^ > { 'c' }
    | LDouble Double    -- ^ > { 12.36 }
    | LString String    -- ^ > { "str" }
  deriving
    ( -- | Use only for debugging
      Show
    , Eq)

type AFixity = Located Fixity
data Fixity
    = Infix Associativity Integer
  deriving
    ( -- | Use only for debugging
      Show
    , Eq)
data Associativity
    = L | R
  deriving
    ( -- | Use only for debugging
      Show
    , Eq)

type AType = Located Type
data Type
    = TId String              -- ^ > { Integer or (→) }
    | TTuple [[AType]]        -- ^ > { (t₁, t₂) }
    | TVar String             -- ^ > { a }
    | TApplication [AType]    -- ^ > { Maybe t }
    | TOperator String        -- ^ > { → }
    | TParens [AType]         -- ^ > { (t) }
  deriving
    ( -- | Use only for debugging
      Show
    , Eq)

type ACustomType = Located CustomType
{-| There are 3 main possible custom types:

    * Data types (also called ”sum types”) are types constructed by data constructors.

    * Types aliases form new names for existing types (because it might be more convenient to write a little name).

    * GADTs (for Generalized Algebraic Data Types) are generalized versions of sum types.
-}
data CustomType
    = SumType (Map.Map String [AType])  -- ^ > data Maybe a = { Just a | Nothing }
    | TypeAlias [AType]                 -- ^ > type String = { List Char }
    | GADT (Map.Map String [AType])     -- ^ > data List a where { Nil : List a ; Cons : a → List a → List a }
  deriving
    ( -- | Use only for debugging
      Show
    , Eq)
