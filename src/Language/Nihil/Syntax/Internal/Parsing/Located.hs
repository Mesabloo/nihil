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

{-# LANGUAGE TemplateHaskell #-}

module Language.Nihil.Syntax.Internal.Parsing.Located where

import Language.Nihil.Syntax.Internal.Lexing.SourceSpan
import Control.Lens (makeLenses)
import Text.Megaparsec (sourceColumn, sourceLine, unPos)

-- | This data type provides a simple way to wrap a type with some 'SourceSpan'.
data Located a
    = (:@) { _located :: a
           , _pos :: Maybe SourceSpan }

makeLenses ''Located

instance Eq a => Eq (Located a) where
    (==) (a1 :@ _) (a2 :@ _) = a1 == a2

instance Ord a => Ord (Located a) where
    (<=) (_ :@ pos1) (_ :@ pos2) = pos1 <= pos2

instance Show a => Show (Located a) where
    show (a :@ Nothing) = show a
    show (a :@ Just (SourceSpan pos' _)) =
        let col  = unPos (sourceColumn pos')
            line = unPos (sourceLine pos')
        in  mconcat ["{", show line, ";", show col, "} ", show a]

instance Functor Located where
    fmap f (a :@ p) = f a :@ p
