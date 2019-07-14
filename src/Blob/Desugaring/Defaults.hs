{-# LANGUAGE LambdaCase #-}

module Blob.Desugaring.Defaults where

import Blob.Desugaring.Types
import qualified Data.Map as Map
import qualified Blob.Parsing.Types as P (Fixity(..), Associativity(..), Atom(..), Expr(..))
import qualified Control.Monad.Combinators.Expr as CMCE
import qualified Data.Set as Set
import Text.Megaparsec (token)
import Blob.Desugaring.Types()
import Blob.Parsing.Annotation

initSugarState :: SugarState
initSugarState =
    SugarState { fixities = Map.fromList [ ("*", P.Infix P.L 7 "*")
                                         , ("/", P.Infix P.L 7 "/")
                                         , ("+", P.Infix P.L 6 "+")
                                         , ("-", P.Infix P.L 6 "-")
                                         , (":", P.Infix P.R 5 ":") ] }