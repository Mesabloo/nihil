module Blob.Language.Parsing.Annotation where

import qualified Text.Megaparsec as Mega
import Text.Megaparsec.Pos (unPos)

type SourceSpan = (Mega.SourcePos, Mega.SourcePos)

data Annotated a = a :- Maybe SourceSpan

getSpan :: Annotated a -> Maybe SourceSpan
getSpan (_ :- s) = s

getAnnotated :: Annotated a -> a
getAnnotated (a :- _) = a

instance Eq a => Eq (Annotated a) where
    (==) (a1 :- _) (a2 :- _) = a1 == a2

instance Ord a => Ord (Annotated a) where
    (<=) (_ :- pos1) (_ :- pos2) = pos1 <= pos2

instance Show a => Show (Annotated a) where
    show (a :- Nothing) = show a
    show (a :- Just (pos, _)) = 
        let col = Mega.sourceColumn pos
            line = Mega.sourceLine pos
        in "{" <> show (unPos line) <> ";" <> show (unPos col) <> "} " <> show a

instance Functor Annotated where
    fmap f (a :- p) = f a :- p

concat :: Annotated a -> Annotated a -> Annotated [a]
concat (a :- Nothing) (b :- _) = [a, b] :- Nothing
concat (a :- _) (b :- Nothing) = [a, b] :- Nothing
concat (a :- Just (beg, _)) (b :- Just (_, end)) = [a, b] :- Just (beg, end)