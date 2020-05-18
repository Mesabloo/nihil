{-# LANGUAGE RankNTypes #-}

{-| A more general alternative to 'Nihil.Utils.Source' permitting to annotate values with any other value type -}
module Nihil.Utils.Annotation where

import Control.Comonad.Cofree
import Control.Comonad (extract)

type Annotated f a = Cofree f a

-- | An alias for 'hoistCofree'.
--
--   It permits to change the base functor of the annotation, implying changing what is annotated,
--   but not the annotation itself.
hoistAnnotated :: (Functor f, Functor g) => (forall x. f x -> g x) -> Annotated f a -> Annotated g a
hoistAnnotated = hoistCofree

-- | A synonym for 'extract'.
--
--   Extracts the annotator value from the annotated object.
extract' :: Functor f => Annotated f a -> a
extract' = extract

-- | A synonym for 'unwrap'.
--
--   Unwraps the first layer of the functor, permitting the access to the value annotated.
unwrap' :: Functor f => Annotated f a -> f (Annotated f a)
unwrap' = unwrap

-- | A synonym for @':<'@.
--
--   Constructs an annotated value from a value and an annotation.
singleton :: Functor f => a -> f (Annotated f a) -> Annotated f a
singleton = (:<)