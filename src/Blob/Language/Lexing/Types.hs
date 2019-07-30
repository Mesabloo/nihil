module Blob.Language.Lexing.Types where

import Text.Megaparsec hiding (State)
import Data.Text
import Data.Void
import Control.Monad.State

type Parser = ParsecT Void Text (State LexState)

data Lexeme
    = LKeyword Text
    | LString Text
    | LInteger Integer
    | LFloat Double
    | LChar Char
    | LSymbol Text
    | LLowIdentifier Text
    | LUpIdentifier Text
    | LWildcard
  deriving (Show, Eq, Ord)

type Token = ( Int            -- ^ Indentation level
             , SourceSpan     -- ^ Position in source file
             , Maybe Lexeme ) -- ^ Lexeme class (possibly none)

data LexState = LexState { currentIndent :: Int }

data SourceSpan = SourceSpan { begin :: SourcePos, end :: SourcePos }
  deriving (Ord, Eq)

instance Show SourceSpan where
    show (SourceSpan (SourcePos _ l1 c1) (SourcePos _ l2 c2)) =
        "(" <> show (unPos l1) <> ":" <> show (unPos c1) <> ") => (" <> show (unPos l2) <> ":" <> show (unPos c2) <> ")"

initLexState :: LexState
initLexState = LexState 0