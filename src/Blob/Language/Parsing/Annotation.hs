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

-- | This module contains the wrapper for annotating tokens with 'SourceSpan's.
module Blob.Language.Parsing.Annotation where

import qualified Text.Megaparsec as Mega
import Text.Megaparsec.Pos (unPos)
import Blob.Language.Lexing.Types (SourceSpan(..))

-- | This data type provides a simple way to wrap a type with some 'SourceSpan'.
data Annotated a = a :- Maybe SourceSpan

-- | This function gets the 'SourceSpan' from an 'Annotated'.
getSpan :: Annotated a -> Maybe SourceSpan
getSpan (_ :- s) = s

-- | This function gets the element wrapped in an 'Annotated'.
getAnnotated :: Annotated a -> a
getAnnotated (a :- _) = a

instance Eq a => Eq (Annotated a) where
    (==) (a1 :- _) (a2 :- _) = a1 == a2

instance Ord a => Ord (Annotated a) where
    (<=) (_ :- pos1) (_ :- pos2) = pos1 <= pos2

instance Show a => Show (Annotated a) where
    show (a :- Nothing) = show a
    show (a :- Just (SourceSpan pos _)) =
        let col = Mega.sourceColumn pos
            line = Mega.sourceLine pos
        in "{" <> show (unPos line) <> ";" <> show (unPos col) <> "} " <> show a

instance Functor Annotated where
    fmap f (a :- p) = f a :- p