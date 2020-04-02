{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}

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
import Control.Lens (makeLenses, (^.))
import qualified Data.Text as Text
import qualified Data.Map as Map
import qualified Data.List.NonEmpty as NonEmpty (toList)
import Text.PrettyPrint.ANSI.Leijen (prettyList)

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
    showTokens _      = show . prettyList . NonEmpty.toList
    reachOffset o pst@MP.PosState{..}
        | o <= 0        =
        let line = show $ prettyList (takeWhile (\(annotated -> t) -> t /= L.TkEOL) $ drop pstateOffset pstateInput)
        in (if null line then "<empty line>" else line, pst)
            | otherwise     =
            let (before, after) = splitAt pstateOffset pstateInput
                
                modifyPos initial NoSource = initial
                modifyPos initial loc      = MP.SourcePos
                    { MP.sourceName   = loc ^. sourceFile
                    , MP.sourceLine   = MP.mkPos (unPos (loc ^. sourceLine))
                    , MP.sourceColumn = MP.mkPos (unPos (loc ^. sourceColumn))
                    }
            in case after of
                    []     -> MP.reachOffset (o - 1) pst
                    t : ts -> MP.reachOffset (o - 1) pst{ MP.pstateInput      = ts
                                                        , MP.pstateOffset     = pstateOffset + 1
                                                        , MP.pstateSourcePos  = modifyPos pstateSourcePos (location t)
                                                        , MP.pstateTabWidth   = pstateTabWidth
                                                        , MP.pstateLinePrefix = pstateLinePrefix
                                                        }
