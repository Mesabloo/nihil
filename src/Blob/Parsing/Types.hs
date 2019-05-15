module Blob.Parsing.Types
  ( Expr(..)
  , Literal(..)
  , Pattern(..)                 -- Expressions
  , Associativity(..)
  , Fixity(..)
  , CustomOperator(..) -- Custom operators
  , Parser
  , ParseState(..)                             -- Global
  , Program(..)
  , Statement(..)                         -- AST
  , Scheme(..)
  , Type(..)
  , CustomType(..)                -- Types
  )
where

import qualified Data.MultiMap                 as MMap
                                                ( MultiMap )
import qualified Data.Map                      as Map
                                                ( Map )
import qualified Text.Megaparsec               as Mega
                                                ( Pos
                                                , Parsec
                                                )
import           Data.Text                      ( Text )
import           Data.Void                      ( Void )
import           Control.Monad.Combinators.Expr ( Operator )
import           Control.Monad.State            ( StateT )

---------------------------------------------------------------------------------------------
{- Global -}

type Parser = StateT ParseState (Mega.Parsec Void Text)

data ParseState = ParseState
    { operators :: MMap.MultiMap Integer (Operator Parser Expr)
    , currentIndent :: Mega.Pos }

---------------------------------------------------------------------------------------------
{- Expressions -}

data Expr = EId String
          | ELit Literal
          | ELam String Expr
          | EApp Expr Expr
          | ETuple [Expr]
          | EList [Expr]
          | EMatch Expr [(Pattern, Expr)]
    deriving (Show, Eq, Ord)

data Pattern = Wildcard         -- _
             | PId String       -- a basic value like `a`
             | PInt Integer     -- a basic value like `0`
             | PDec Double      -- a basic value like `0.0`
             | PStr String      -- a basic value like `"0"`
             | PTuple [Pattern] -- a basic value like `(a, b)`
             | PList [Pattern]  -- a basic value like `[a, b]`
    deriving (Show, Eq, Ord)

data Literal = LStr String
             | LInt Integer
             | LDec Double
    deriving (Show, Eq, Ord)

data Associativity = L
                   | R
                   | N
    deriving (Show, Eq, Ord)

data Fixity = Infix' Associativity Integer
            | Prefix' Integer
            | Postfix' Integer
    deriving (Show, Eq, Ord)

data CustomOperator = CustomOperator { name :: Text
                                     , fixity :: Fixity }
    deriving (Show, Eq, Ord)


---------------------------------------------------------------------------------------------
{- AST -}

newtype Program = Program [Statement]
    deriving (Eq, Ord, Show)

data Statement = Declaration String Type
               | Definition String Expr
               | OpDeclaration String Fixity
               | TypeDeclaration String [String] CustomType
               | Empty -- Just a placeholder, when a line is a comment, for example.
    deriving (Eq, Ord, Show)

---------------------------------------------------------------------------------------------
{- Types -}

data Scheme = Scheme [String] Type
    deriving (Eq, Ord, Show)

data Type = TId String            -- Type
          | TTuple [Type]         -- (a, ...)
          | TArrow Expr Type Type -- a ->{n} b -o ...
          | TFun Type Type
          | TVar String           -- a...
          | TApp Type Type        -- Type a...
          | TList                 -- []
    deriving (Eq, Ord, Show)

data CustomType = TSum (Map.Map String Scheme) | TProd String Scheme | TAlias Type
    deriving (Eq, Ord, Show)
