{-# LANGUAGE FlexibleInstances, TypeFamilies, RecordWildCards #-}

module Blob.Language.Desugaring.Types where

import qualified Data.Map as Map
import Control.Monad.State
import Control.Monad.Except
import qualified Blob.Language.Parsing.Types as P (Fixity)
import Text.PrettyPrint.Leijen (Doc)
import Blob.Language.Parsing.Annotation

data Expr = EId String
          | ELit Literal
          | ELam (Annotated Pattern) (Annotated Expr)
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
             | PAnn (Annotated Pattern) (Annotated Type)
             | PLinear (Annotated Pattern)
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
          | TFun (Annotated Type) (Annotated Type)
          | TRVar String           -- a...
          | TVar String
          | TApp (Annotated Type) (Annotated Type)        -- Type a...
          | TBang (Annotated Type)              -- !a
    deriving (Eq, Ord, Show)

data CustomType = TSum (Map.Map String Scheme) | TAlias (Annotated Type)
    deriving (Eq, Ord, Show)

---------------------------------------------------------------------------------------------

type Sugar = StateT SugarState (Except Doc)

newtype SugarState = SugarState { fixities :: Map.Map String P.Fixity }