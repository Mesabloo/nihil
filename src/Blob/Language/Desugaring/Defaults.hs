{-# LANGUAGE LambdaCase #-}

-- | This module holds the default definition of constants used in the desugaring process.
module Blob.Language.Desugaring.Defaults where

import Blob.Language.Desugaring.Types
import qualified Data.Map as Map
import qualified Blob.Language.Parsing.Types as P (Fixity(..), Associativity(..))

-- | The default desugarer state
initSugarState :: SugarState
initSugarState =
    SugarState { _fixities = Map.fromList [ ("*", P.Infix P.L 7 "*") -- infixl 7 *
                                          , ("/", P.Infix P.L 7 "/") -- infixl 7 /
                                          , ("+", P.Infix P.L 6 "+") -- infixl 6 +
                                          , ("-", P.Infix P.L 6 "-") -- infixl 6 -
                                          , (":", P.Infix P.R 5 ":") -- infixr 5 :
                                          ]
               }