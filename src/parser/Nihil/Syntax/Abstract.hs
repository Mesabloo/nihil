{-| The first abstract syntax representation is the desugared version of the "Nihil.Syntax.Concrete" representation.

-}

module Nihil.Syntax.Abstract
( -- * List of transformations:
  -- $transformations

  -- * Re-exports
  module Nihil.Syntax.Abstract.Core
, accumulateOnProgram
, desugarProgram
) where

import Nihil.Syntax.Abstract.Core
import Nihil.Syntax.Abstract.Accumulator (accumulateOnProgram)
import Nihil.Syntax.Abstract.Desugarer.Statement (desugarProgram)

{- $transformations

    * Function parameters are expanded to lambda abstractions.

        > id x = x

        expands to

        > id = λ x → x

    * Inlined patterns and “equational pattern-matching” are converted to explicit pattern matching using @match@.

        > f 0 _ = 0
        > f _ m = m

        expands to

        > f %₁ %₂ = match (%₁, %₂) with
        >    (0, _) → 0
        >    (_, m) → m

        and further expands to two lambda abstractions

        > f = λ %₁ → λ %₂ → match (%₁, %₂) with
        >    (0, _) → 0
        >    (_, m) → m

    * Operators in expressions are transformed into function applications dependening on their fixity.

        > f x = x + 1

        expands to

        > f x = (+) x 1

        and further expands to a lambda abstraction

        > f = λ x → (+) x 1

    * Data Types are transformed into Generalized versions.

        > data Maybe a = Just a | Nothing

        expands to

        > data Maybe a where
        >    Just    : a → Maybe a
        >    Nothing : Maybe a

        and further expands to

        > data Maybe a where
        >    Just    : ∀ a. (→) a (Maybe a)
        >    Nothing : ∀ a. Maybe a

    * @where@ expressions are transformed into @let@ expressions.

        > f = g
        >  where g = h
        >        h = 0 + 3

        expands to

        > f = let g = h
        >         h = 0 + 3
        >     in g

    * Strings are transformed into lists of characters, as it is what they are in the first place.

        > f = "hello"

        expands to

        > f = 'h' `Cons` 'e' `Cons` 'l' `Cons` 'l' `Cons` 'o' `Cons` Nil

        where lists are implemented internally as

        > data List a = Nil | Cons a (List a)
-}