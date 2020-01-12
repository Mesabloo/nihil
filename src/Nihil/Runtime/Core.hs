{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Nihil.Runtime.Core where

import qualified Nihil.Syntax as AC
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.Reader (ReaderT)
import Control.Monad.Except (ExceptT)
import Text.PrettyPrint.ANSI.Leijen (Doc)
import Control.Lens (makeLenses)

data Value
    = VInteger Integer
    | VDouble Double
    | VCharacter Char
    | VId String
    | VLambda AC.Pattern AC.Expr (Scope Value)
    | VTuple [Value]
    | VPrim (Value -> Eval Value)
    | VConstructor String [Value]

newtype Scope e = Scope { unwrap :: Map.Map String e }
  deriving (Monoid, Semigroup)

type Eval a = ReaderT EvalState (ExceptT Doc IO) a

data EvalState
    = EState
    { _vals :: Scope Value
    , _cons :: Set.Set String }
makeLenses ''EvalState

lookup :: String -> Scope e -> Maybe e
lookup k = Map.lookup k . unwrap