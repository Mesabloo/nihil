{-# LANGUAGE LambdaCase #-}

module Blob.Language.Desugaring.Defaults where

import Blob.Language.Desugaring.Types
import qualified Data.Map as Map
import qualified Blob.Language.Parsing.Types as P (Fixity(..), Associativity(..))

initSugarState :: SugarState
initSugarState =
    SugarState { fixities = Map.fromList [ ("*", P.Infix P.L 7 "*")
                                         , ("/", P.Infix P.L 7 "/")
                                         , ("+", P.Infix P.L 6 "+")
                                         , ("-", P.Infix P.L 6 "-")
                                         , (":", P.Infix P.R 5 ":") ] }