{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

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
import Nihil.Utils.Source
import Nihil.Syntax.Concrete.Lexer (Token)
import Nihil.Syntax.Pretty()
import qualified Nihil.Syntax.Concrete.Lexer as L
import qualified Text.Megaparsec as MP
import Data.Void (Void)
import Control.Monad.State
import Control.Monad.Except
import Control.Lens (makeLenses)
import qualified Data.Text as Text
import qualified Data.Map as Map
import qualified Data.List.NonEmpty as NonEmpty (toList)

type Parser = MP.Parsec Void [Token]

type Desugarer a = StateT DesugarerState (Except (Diagnostic Text.Text)) a

-- | A type alias for the map retaining operators information when desugaring.
type OperatorTable = Map.Map String (Associativity, Integer)

data DesugarerState
    = DState
    { _typeLevelOperators    :: OperatorTable
    , _valueLevelOperators   :: OperatorTable
    , _patternLevelOperators :: OperatorTable
    }
makeLenses ''DesugarerState

instance MP.Stream [L.Token] where
    type Token [L.Token] = L.Token
    type Tokens [L.Token] = [L.Token]

    tokenToChunk _    = pure
    tokensToChunk _   = id
    chunkToTokens _   = id
    chunkLength _     = length
    chunkEmpty _      = null
    take1_ []         = Nothing
    take1_ (t : ts)   = Just (t, ts)
    takeN_ n s
        | n <= 0        = Just ([], s)
        | null s        = Nothing
        | otherwise     = Just (splitAt n s)
    takeWhile_        = span
    showTokens _      = show . NonEmpty.toList
    reachOffset o MP.PosState{..} =
        let (before, after) = splitAt (o - pstateOffset) pstateInput

            actualisePos initial NoSource = initial
            actualisePos _       Loc{..}  = MP.SourcePos
                { MP.sourceName   = _sourceFile
                , MP.sourceColumn = MP.mkPos (unPos _sourceColumn)
                , MP.sourceLine   = MP.mkPos (unPos _sourceLine)
                }

            tokenPos = case after of
                []    -> NoSource
                t : _ -> location t

            newPos = MP.PosState
                { MP.pstateInput      = after
                , MP.pstateOffset     = max pstateOffset o
                , MP.pstateSourcePos  = actualisePos pstateSourcePos tokenPos
                , MP.pstateTabWidth   = pstateTabWidth
                , MP.pstateLinePrefix = pstateLinePrefix}

            notEOL (annotated -> t) = t /= L.TkEOL

            fetchedLine = show $ reverse (takeWhile notEOL (reverse before)) <> takeWhile notEOL after

         in (fetchedLine, newPos)
