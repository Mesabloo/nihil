{-# LANGUAGE TemplateHaskell #-}

-- | This module contains all the types used in the lexing process of Blob.
module Blob.Language.Lexing.Types where

import Text.Megaparsec hiding (State)
import Data.Text
import Data.Void
import Control.Monad.State
import Control.Lens

-- | The 'Parser' monad, holding a state for various information.
type Parser = ParsecT Void Text (State LexState)

-- | The 'Lexeme' class, which is either:
data Lexeme
    = LKeyword Text        -- ^ a keyword
    | LString Text         -- ^ a string | @"string"@
    | LInteger Integer     -- ^ an integer | @0@
    | LFloat Double        -- ^ a floating point number | @0.0@
    | LChar Char           -- ^ a character | \'c\'
    | LSymbol Text         -- ^ a symbol | most likely to be an operator
    | LLowIdentifier Text  -- ^ an identifier beginning with a lowercase letter | function or constant name
    | LUpIdentifier Text   -- ^ an identifier beginning with an uppercase letter | a type name or data type constructor
    | LWildcard            -- ^ the wildcard pattern | @_@
  deriving (Show, Eq, Ord)

-- | A type alias representing a token, regrouping the following information:
-- The current line indentation level
-- The token position in the file
-- The token class (possibly none)
type Token = ( Int
             , SourceSpan
             , Maybe Lexeme
             )

-- | The state of the 'Parser' monad.
data LexState
    = LexState { _currentIndent :: Int -- ^ the indentation level of the current line
               }

-- | A simple record used to hold the position of a token in the source file.
data SourceSpan
    = SourceSpan { begin :: SourcePos -- ^ the beginning position of the source span
                 , end :: SourcePos   -- ^ the end position of the source span
                 }
  deriving (Ord, Eq)

makeLenses ''LexState

instance Show SourceSpan where
    show (SourceSpan (SourcePos _ l1 c1) (SourcePos _ l2 c2)) =
        "(" <> show (unPos l1) <> ":" <> show (unPos c1) <> ") => (" <> show (unPos l2) <> ":" <> show (unPos c2) <> ")"

-- | The default state of the lexer.
initLexState :: LexState
initLexState = LexState 0