{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Nihil.Runtime.Core where

import qualified Nihil.Syntax.Abstract.Core as AC
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.Reader (ReaderT)
import Control.Monad.Except (ExceptT)
import Text.PrettyPrint.ANSI.Leijen (Doc)
import Control.Lens (makeLenses)

data Value
    = VInteger Integer                            -- ^ > { 0 }
    | VDouble Double                              -- ^ > { 0.0 }
    | VCharacter Char                             -- ^ > { 'c' }
    | VId String                                  -- ^ > { f }
    | VLambda AC.Pattern AC.Expr (Scope Value)    -- ^ > { λ p → e }
    | VTuple [Value]                              -- ^ > { (e₁, e₂) }
    | VPrim (Value -> Eval Value)                 -- ^ Primitive (built-in) functions
    | VConstructor String [Value]                 -- ^ > { Con e₁ e₂ }
    | VUnevaluated AC.Expr

newtype Scope e = Scope { unwrap :: Map.Map String e }
  deriving (Monoid, Semigroup)

type Eval a = ReaderT EvalState (ExceptT Doc IO) a

data EvalState
    = EState
    { _vals :: Scope Value
    , _cons :: Set.Set String }
makeLenses ''EvalState

-- | Looks up an entry in a 'Scope' knowing its name.
lookup :: String -> Scope e -> Maybe e
lookup k = Map.lookup k . unwrap

insert :: (String, e) -> Scope e -> Scope e
insert (k, v) = Scope . Map.insert k v . unwrap