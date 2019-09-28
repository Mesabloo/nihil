-- | This module holds the types for the kind checking process
module Blob.Language.KindChecking.Types where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Text.PrettyPrint.Leijen (Doc)
import Control.Monad.State (State)
import Control.Monad.Reader (ReaderT)
import Control.Monad.Except (ExceptT)
import Data.Maybe (fromMaybe)
import Blob.Language.TypeChecking.Types (Kind(..), KindEnv)

-- | The error type (convenient for showing).
type KIError = Doc

-- | The state used in the kind checking.
newtype KIState
    = KIState { kiSupply :: Int -- ^ The index for kind name generation
              }

-- | The 'KI' monad is used for the kind checking.
type KI a = ExceptT KIError (ReaderT KindEnv (State KIState)) a

-- | The substitution type.
--
-- Its use is to store the kind of each type name.
type KindSubst = Map.Map String Kind

-- | This function returns the "free kind variables" (i.e. the kind variable able to be substituted).
fkv :: Kind -> Set.Set String
fkv (KVar n) = Set.singleton n
fkv (KArr k1 k2) = fkv k1 `Set.union` fkv k2
fkv KType = mempty

-- | This function applies the substitution on a kind.
applyKind :: KindSubst -> Kind -> Kind
applyKind s (KVar n) = fromMaybe (KVar n) (Map.lookup n s)
applyKind s (KArr k1 k2) = KArr (applyKind s k1) (applyKind s k2)
applyKind _ KType = KType
