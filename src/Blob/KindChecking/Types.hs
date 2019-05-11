module Blob.KindChecking.Types where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Text.PrettyPrint.Leijen (Doc)
import Control.Monad.State (State)
import Control.Monad.Reader (ReaderT)
import Control.Monad.Except (ExceptT)
import Data.Maybe (fromMaybe)
import Blob.Inference.Types (CustomTypeEnv, Kind(..), KindEnv)

type KIError = Doc

data KIState = KIState
    { kiSupply :: Int }

type KI a = ExceptT KIError (ReaderT KindEnv (State KIState)) a

type KindSubst = Map.Map String Kind

fkv :: Kind -> Set.Set String
fkv (KVar n) = Set.singleton n
fkv (KArr k1 k2) = fkv k1 `Set.union` fkv k2
fkv KType = mempty

applyKind :: KindSubst -> Kind -> Kind
applyKind s (KVar n) = fromMaybe (KVar n) (Map.lookup n s)
applyKind s (KArr k1 k2) = KArr (applyKind s k1) (applyKind s k2)
applyKind _ KType = KType
