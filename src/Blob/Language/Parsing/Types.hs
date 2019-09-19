{-# LANGUAGE TypeFamilies, RecordWildCards, FlexibleInstances #-}

module Blob.Language.Parsing.Types where

import qualified Data.Map as Map
import Blob.Language.Parsing.Annotation
import Blob.Language.Lexing.Types (SourceSpan, Lexeme)
import Text.Megaparsec
import Data.Proxy
import Data.Void

type Parser = Parsec Void [TokenL]

type TokenL = (Int, SourceSpan, Lexeme)

instance Stream [TokenL] where
    type Token [TokenL] = TokenL
    type Tokens [TokenL] = [TokenL]

    tokensToChunk Proxy = id
    chunkToTokens Proxy = id
    chunkLength Proxy = length
    chunkEmpty Proxy = null
    take1_ [] = Nothing
    take1_ (x:xs) = Just (x, xs)
    takeN_ n s | n <= 0 = Nothing
               | n > length s = Just (s, [])
               | otherwise = Just (splitAt n s)
    takeWhile_ = span
    showTokens Proxy = concatMap show
    reachOffset n p | n <= 0 = (pstateSourcePos p, "placeholder, will not be shown.", p)
                    | otherwise = reachOffset (n - 1) (f p)
      where f ps = PosState (if null (pstateInput ps) then [] else let _:xs = pstateInput ps in xs)
                            (pstateOffset ps + fromEnum (null (pstateInput ps)))
                            (increaseSourcePos (pstateSourcePos ps) (fromEnum . null $ pstateInput ps))
                            (pstateTabWidth ps)
                            (pstateLinePrefix ps)

            increaseSourcePos sp n' = SourcePos (sourceName sp) (sourceLine sp) (mkPos $ unPos (sourceColumn sp) + n')
-- ? Causes a warning, which will not be fixed

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

data CustomType = TSum (Map.Map String [Annotated Type]) | TAlias (Annotated Type) | TGADT (Map.Map String (Annotated Type))
    deriving (Show, Eq, Ord)