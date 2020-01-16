{-# LANGUAGE TemplateHaskell #-}

{-| Common data types used throughout the parsing process. -}

module Nihil.Syntax.Common
( -- * Lexer
  Lexer
, LexerState(LState)
, currentIndent
  -- * Parser
, Parser
  -- * Desugarer
, Desugarer
, DesugarerState(DState)
, OperatorTable
, typeLevelOperators
, valueLevelOperators
, patternLevelOperators
 ) where

import Nihil.Syntax.Concrete.Lexeme (ALexeme)
import Nihil.Syntax.Concrete.Core (Associativity)
import Text.Megaparsec (ParsecT, Parsec)
import Data.Void (Void)
import Control.Monad.State
import Control.Monad.Except
import Control.Lens (makeLenses)
import qualified Data.Text as Text
import qualified Data.Map as Map
import Text.PrettyPrint.ANSI.Leijen (Doc)

type Lexer a = ParsecT Void Text.Text (State LexerState) a

newtype LexerState
    = LState
    { _currentIndent :: Int -- ^ The current indentation level when lexing. Starts at 0.
    }
-- | A lens for getting the current indentation level, or setting it to a new value.
makeLenses ''LexerState

type Parser a = Parsec Void [ALexeme] a

type Desugarer a = StateT DesugarerState (Except Doc) a

-- | A type alias for the map retaining operators information when desugaring.
type OperatorTable = Map.Map String (Associativity, Integer)

data DesugarerState
    = DState
    { _typeLevelOperators    :: OperatorTable
    , _valueLevelOperators   :: OperatorTable
    , _patternLevelOperators :: OperatorTable
    }
makeLenses ''DesugarerState