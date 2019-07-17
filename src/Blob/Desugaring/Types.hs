{-# LANGUAGE FlexibleInstances, TypeFamilies, RecordWildCards #-}

module Blob.Desugaring.Types where

import qualified Data.Map as Map
import Control.Monad.State
import Control.Monad.Except
import qualified Blob.Parsing.Types as P (Fixity, Atom, Expr)
import Text.PrettyPrint.Leijen (Doc)
import qualified Text.Megaparsec as Mega
import Data.Void
import Control.Monad.Combinators.Expr
import Data.Proxy
import Blob.Parsing.Annotation

data Expr = EId String
          | ELit Literal
          | ELam String (Annotated Expr)
          | EApp (Annotated Expr) (Annotated Expr)
          | ETuple [Annotated Expr]
          | EMatch (Annotated Expr) [(Annotated Pattern, Annotated Expr)]
          | EHole
          | EAnn (Annotated Expr) (Annotated Type)
    deriving (Show, Eq, Ord)

data Pattern = Wildcard               -- _
             | PId String             -- a basic value like `a`
             | PInt Integer           -- a basic value like `0`
             | PDec Double            -- a basic value like `0.0`
             | PChr Char              -- a basic value like `'a'`
             | PTuple [Annotated Pattern]       -- a basic value like `(a, b)`
             | PCtor String [Annotated Pattern] -- a basic value like `Just a`
    deriving (Show, Eq, Ord)

data Literal = LInt Integer
             | LDec Double
             | LChr Char
    deriving (Show, Eq, Ord)

---------------------------------------------------------------------------------------------
{- AST -}

newtype Program = Program [Annotated Statement]
    deriving (Eq, Ord, Show)

getStatements :: Program -> [Annotated Statement]
getStatements (Program s) = s

data Statement = Declaration String (Annotated Type)
               | Definition String (Annotated Expr)
               | TypeDeclaration String [String] (Annotated CustomType)
               | Empty
    deriving (Eq, Ord, Show)

---------------------------------------------------------------------------------------------
{- Types -}

data Scheme = Scheme [String] (Annotated Type)
    deriving (Eq, Ord, Show)

data Type = TId String            -- Type
          | TTuple [Annotated Type]         -- (a, ...)
          | TArrow (Annotated Expr) (Annotated Type) (Annotated Type) -- a ->{n} b -o ...
          | TFun (Annotated Type) (Annotated Type)
          | TVar String           -- a...
          | TApp (Annotated Type) (Annotated Type)        -- Type a...
    deriving (Eq, Ord, Show)

data CustomType = TSum (Map.Map String Scheme) | TAlias (Annotated Type)
    deriving (Eq, Ord, Show)

---------------------------------------------------------------------------------------------

type Sugar = StateT SugarState (Except Doc)

newtype SugarState = SugarState { fixities :: Map.Map String P.Fixity }