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