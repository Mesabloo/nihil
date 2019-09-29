{-# LANGUAGE TypeFamilies, RecordWildCards, FlexibleInstances #-}

-- | This module holds the types for the parsing step.
module Blob.Language.Parsing.Types where

import qualified Data.Map as Map
import Blob.Language.Parsing.Annotation
import Blob.Language.Lexing.Types (SourceSpan, Lexeme)
import Text.Megaparsec
import Data.Proxy
import Data.Void

-- | A simple parser working with a stream of tokens.
type Parser = Parsec Void [TokenL]

-- | A basic token type (should be the one from 'Blob.Language.Lexing.Types')
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

-- | A simple program AST node containing many statement nodes.
type Program = [Annotated Statement]

-- | A simple statement AST node, which may be:
data Statement
    = Declaration String (Annotated Type)                     -- ^ A function declaration | @name :: type@
    | Definition String [Annotated Pattern] (Annotated Expr)  -- ^ A function definition | @name args = expression@
    | OpFixity String (Annotated Fixity)                      -- ^ An operator precedence declaration | @infixr 3 (++)@
    | TypeDeclaration String [String] (Annotated CustomType)  -- ^ A type declaration | @data X = Y@ or @type X = Y@
    | Empty                                                   -- ^ An unsupported kind of statement
    deriving (Show, Eq, Ord)

-- | A simple expression AST node containing many atom nodes.
type Expr = [Annotated Atom]

-- | A simple atom AST node, which can be:
data Atom
    = ALit Literal                                                     -- ^ A literal
    | AId String                                                       -- ^ An identifier
    | AOperator String                                                 -- ^ An operator
    | AList [Annotated Expr]                                           -- ^ A list
    | ATuple [Annotated Expr]                                          -- ^ A tuple
    | AHole                                                            -- ^ A type hole
    | ALambda [Annotated Pattern] (Annotated Expr)                     -- ^ An anonymous function (lambda function)
    | AMatch (Annotated Expr) [([Annotated Pattern], Annotated Expr)]  -- ^ A @match@ expression
    | AParens (Annotated Expr)                                         -- ^ A parenthesized expression
    | AApp (Annotated Atom) (Annotated Atom)                           -- ^ A function application
    | AAnn (Annotated Expr) (Annotated Type)                           -- ^ A type-annotated expression
    | ALet [Annotated Statement] (Annotated Expr)                      -- ^ A @let@ expression
    | AWhere (Annotated Expr) [Annotated Statement]                    -- ^ A @where@ expression
    deriving (Show, Ord, Eq)

-- | A simple pattern AST node, which might be:
data Pattern
    = PId String                                 -- ^ An identifier
    | PLit Literal                               -- ^ A literal
    | PCtor String [[Annotated Pattern]]         -- ^ A data type constructor
    | PTuple [[Annotated Pattern]]               -- ^ A tuple
    | PList [[Annotated Pattern]]                -- ^ A list
    | PHole                                      -- ^ A wildcard
    | PParens [Annotated Pattern]                -- ^ A parenthesized pattern
    | POperator String                           -- ^ An operator
    | PAnn [Annotated Pattern] (Annotated Type)  -- ^ A type-annotated pattern
    | PLinear (Annotated Pattern)                -- ^ A non-linear pattern
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
    | TTuple [Annotated Type]                 -- ^ A tuple
    | TList [Annotated Type]                  -- ^ A list
    | TFun (Annotated Type) (Annotated Type)  -- ^ A linear function
    | TVar String                             -- ^ A rigid type variable
    | TApp [Annotated Type]                   -- ^ A type application
    | TNonLinear (Annotated Type)             -- ^ A non-linear type
    deriving (Show, Eq, Ord)

-- | A custom type AST node, which might be:
data CustomType
    = TSum (Map.Map String [Annotated Type])   -- ^ A sum type
    | TAlias (Annotated Type)                  -- ^ A type alias
    | TGADT (Map.Map String (Annotated Type))  -- ^ A GADT (Generalized ADT)
    deriving (Show, Eq, Ord)