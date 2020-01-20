{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}

module Nihil.Syntax.Concrete.Lexeme where

import Nihil.Utils.Source (Located)
import qualified Data.Text as Text
import qualified Text.Megaparsec as MP

-- | Lexemes (or also knwon as tokens) are little pieces of a program, which, when regrouped,
--   form some sort of statements.
data Lexeme
    = LKeyword Text.Text            -- ^ ex: @match@
    | LString Text.Text             -- ^ ex: @"string"@
    | LInteger Integer              -- ^ ex: @23660565@
    | LFloat Double                 -- ^ ex: @3.0226@
    | LChar Char                    -- ^ ex: @\'c\'@
    | LSymbol Text.Text             -- ^ ex: @\<=\>@
    | LLowerIdentifier Text.Text    -- ^ ex: @function@
    | LUpperIdentifier Text.Text    -- ^ ex: @Constructor@
    | LUnderscore                   -- ^ ex: @_@
  deriving (Eq, Show)

-- | Type alias because it's more convenient than writing the whole type everywhere.
type ALexeme = Located Lexeme

newtype Token
    = Token (Maybe ALexeme) -- ^ If there is 'Nothing' in the token, it means that nothing has been tokenized
                            --   (most likely because of an end of line, or an error).
  deriving Eq

-- | Use only for debugging purposes
instance Show Token where
    show (Token l) = maybe "" show l

instance MP.Stream [ALexeme] where
    type Token [ALexeme]  = ALexeme
    type Tokens [ALexeme] = [ALexeme]

    tokensToChunk _ = id
    chunkToTokens _ = id
    chunkLength _   = length
    chunkEmpty _    = null
    take1_ []       = Nothing
    take1_ (x:xs)   = Just (x, xs)
    takeN_ n s | n <= 0       = Nothing
               | n > length s = Just (s, [])
               | otherwise    = Just (splitAt n s)
    takeWhile_      = span
    showTokens _    = concatMap show
    reachOffset n p | n <= 0    = (MP.pstateSourcePos p, "<error>", p)
                    | otherwise = MP.reachOffset (n - 1) (advance p)
      where advance MP.PosState{..} = MP.PosState
                (if null pstateInput then [] else tail pstateInput)
                (pstateOffset + fromEnum (null pstateInput))
                (increaseSourcePos pstateSourcePos (fromEnum (null pstateInput)))
                pstateTabWidth
                pstateLinePrefix

            increaseSourcePos sp@MP.SourcePos{..} n' = sp { MP.sourceColumn = MP.mkPos (MP.unPos sourceColumn + n') }