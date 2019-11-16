-- Blobc, a compiler for compiling Blob source code
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

{-# LANGUAGE TemplateHaskell #-}

module Blob.Language.Syntax.Internal.Lexing.SourceSpan where

import Control.Lens (makeLenses)
import Text.Megaparsec (SourcePos(..), unPos)


-- | A simple record used to hold the position of a token in the source file.
data SourceSpan
    = SourceSpan { _begin :: SourcePos -- ^ the beginning position of the source span
                 , _end :: SourcePos   -- ^ the end position of the source span
                 }
  deriving (Ord, Eq)

makeLenses ''SourceSpan

instance Show SourceSpan where
    show (SourceSpan (SourcePos _ l1 c1) (SourcePos _ l2 c2)) =
        let line1 = unPos l1
            line2 = unPos l2
            col1  = unPos c1
            col2  = unPos c2
        in mconcat ["[", show line1, ";", show col1, " => ", show line2, ";", show col2, "]"]
