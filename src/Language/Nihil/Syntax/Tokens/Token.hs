-- The Great Nihil Compiler
-- Copyright (c) 2019 Mesabloo

-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.

-- You should have received a copy of the GNU General Public License
-- along with this program. If not, see <http://www.gnu.org/licenses/>.

{-# LANGUAGE FlexibleInstances, TypeFamilies, LambdaCase #-}

module Language.Nihil.Syntax.Tokens.Token where

import Language.Nihil.Syntax.Tokens.Lexeme
import Language.Nihil.Syntax.Internal.Lexing.SourceSpan
import qualified Text.Megaparsec as M (Token, Tokens)
import Text.Megaparsec hiding (Token)
import Data.Proxy
import Control.Lens ((^.))

-- | A type alias representing a token, regrouping the following information:
--
-- - The current line indentation level
--
-- - The token position in the file
--
-- - The token class (possibly none)
data Token = Token Int SourceSpan (Maybe Lexeme)
  deriving (Eq, Ord)

getIndentationLevel :: Token -> Int
getIndentationLevel (Token i _ _) = i

getSourceSpan :: Token -> SourceSpan
getSourceSpan (Token _ s _) = s

getLexeme :: Token -> Maybe Lexeme
getLexeme (Token _ _ l) = l

instance Show Token where
  show (Token _ span' l) =
      let (SourcePos file lbeg cbeg) = span' ^. begin
          getToken = \case
              Nothing -> "EOL"
              Just x  -> show x
      in getToken l <> " at " <> file <> ":" <> show (unPos lbeg) <> ":" <> show (unPos cbeg)

----------------------------------------------------------------

instance Stream [Token] where
    type Token [Token] = Token
    type Tokens [Token] = [Token]

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
    reachOffset n p | n <= 0 = (pstateSourcePos p, "<error>", p)
                    | otherwise = reachOffset (n - 1) (f p)
      where f ps = PosState (if null (pstateInput ps) then [] else let _:xs = pstateInput ps in xs)
                            (pstateOffset ps + fromEnum (null (pstateInput ps)))
                            (increaseSourcePos (pstateSourcePos ps) (fromEnum . null $ pstateInput ps))
                            (pstateTabWidth ps)
                            (pstateLinePrefix ps)

            increaseSourcePos sp n' = SourcePos (sourceName sp) (sourceLine sp) (mkPos $ unPos (sourceColumn sp) + n')
-- ? Causes a warning, which will not be fixed