{-| The abstract core represents the concrete core after being desugarized. At this point,it cannot be
    translated back to what the user inputted.
-}

module Nihil.Syntax.Abstract.Core where

import Nihil.Utils.Source
import qualified Data.Map as Map

newtype Program = Program [Statement]
  deriving
    ( -- | use only for debugging
      Show
    , Eq )

type Statement = Located Statement'
data Statement'
    = FunctionDeclaration String Type               -- ^ > { f : t }
    | FunctionDefinition String Expr                -- ^ > { f = e }
    | TypeDefinition String [String] CustomType     -- ^ > { type F s = s } or { data X a = X a }
  deriving
    ( -- | use only for debugging
      Show
    , Eq )

type Expr = Located Expr'
data Expr'
    = EId String                        -- ^ > { x }
    | ELiteral Literal                  -- ^ > { 0 or 'c' }
    | ELambda Pattern Expr              -- ^ > { λ p → e }
    | EApplication Expr Expr            -- ^ > { e₁ e₂ }
    | ETuple [Expr]                     -- ^ > { (e₁, e₂) }
    | EMatch Expr [(Pattern, Expr)]     -- ^ > { match e₁ with p₁ → e₂ }
    | ETypeHole                         -- ^ > { _ }
    | ETypeAnnotated Expr Type          -- ^ > { e : t }
    | ELet [Statement] Expr             -- ^ > { let x = y in e }
    | ERecord [Statement]               -- ^ > { { id x = x ; f = 0 } }
  deriving
    ( -- | use only for debugging
      Show
    , Eq )

type Pattern = Located Pattern'
data Pattern'
    = PWildcard                         -- ^ > { _ }
    | PId String                        -- ^ >
    | PLiteral Literal                  -- ^ > { 9 or 'p' }
    | PTuple [Pattern]                  -- ^ > { (p₁, p₂) }
    | PTypeAnnotated Pattern Type       -- ^ > { p : t }
    | PConstructor String [Pattern]     -- ^ > { Cons p₁ p₂ p₃ }
  deriving
    ( -- | use only for debugging
      Show
    , Eq )

type Type = Located Type'
data Type'
    = TId String                -- ^ > { Type }
    | TTuple [Type]             -- ^ > { (t₁, t₂) }
    | TVar String               -- ^ > { a }
    | TApplication Type Type    -- ^ > { t₁ t₂ }
    | TRow [Statement] (Maybe Type)
                                -- ^ > { { f: Integer | r } }
    | TRecord Type              -- ^ > { Π{ f: Integer | r } }
  deriving
    ( -- | use only for debugging
      Show
    , Eq )

data Literal
    = LInteger Integer      -- ^ > { 0 }
    | LFloat Double         -- ^ > { 06.9 }
    | LCharacter Char       -- ^ > { 'c' }
  deriving
    ( -- | use only for debugging
      Show
    , Eq )

type CustomType = Located CustomType'
data CustomType'
    = SumType (Map.Map String Scheme)   -- ^ > data X a where { X : a → X a }
    | TypeAlias Type                    -- ^ > type T a = { List a }
    | Record (Map.Map String Scheme)    -- ^ > record W b where { f : b }
  deriving
    ( -- | use only for debugging
      Show
    , Eq )

data Scheme = Forall [String] Type  -- ^ > { forall a b c. t }
  deriving
    ( -- | use only for debugging
      Show
    , Eq )
