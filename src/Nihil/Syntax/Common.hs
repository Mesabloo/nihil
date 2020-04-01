{-# LANGUAGE TemplateHaskell #-}

{-| Common data types used throughout the parsing process. -}

module Nihil.Syntax.Common
( -- * Parser
  Parser
  -- * Desugarer
, Desugarer
, DesugarerState(DState)
, OperatorTable
, typeLevelOperators
, valueLevelOperators
, patternLevelOperators
 ) where

import Nihil.Syntax.Concrete.Core (Associativity)
import Nihil.CommonError
import Nihil.Syntax.Concrete.Lexer (Token)
import Text.Megaparsec (Parsec)
import Data.Void (Void)
import Control.Monad.State
import Control.Monad.Except
import Control.Lens (makeLenses)
import qualified Data.Text as Text
import qualified Data.Map as Map

type Parser = Parsec Void Text.Text

type Desugarer a = StateT DesugarerState (Except (Diagnostic Token)) a

-- | A type alias for the map retaining operators information when desugaring.
type OperatorTable = Map.Map String (Associativity, Integer)

data DesugarerState
    = DState
    { _typeLevelOperators    :: OperatorTable
    , _valueLevelOperators   :: OperatorTable
    , _patternLevelOperators :: OperatorTable
    }
makeLenses ''DesugarerState
