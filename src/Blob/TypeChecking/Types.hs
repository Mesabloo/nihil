{-# LANGUAGE TypeFamilies, TypeSynonymInstances #-}

module Blob.TypeChecking.Types where

import qualified Data.Map as Map
import qualified Data.Map.Unordered as UMap
import qualified Data.Set as Set
import qualified Data.List as List
import Text.PrettyPrint.Leijen (Doc)
import Control.Monad.Except (Except, ExceptT)
import Control.Monad.Reader (ReaderT)
import Control.Monad.State (StateT, State)
import Data.Maybe (fromMaybe)
import Data.Key (Key(..), Keyed(..))
import Data.Align.Key (AlignWithKey(..))
import Data.Align (Align(..))
import Data.Hashable (Hashable(..))
import Data.These(These(..))
import Data.Composition ((.:))
import Data.Unique (Unique)
import Control.Monad.RWS
import Control.Monad.Identity
import Data.Bifunctor

newtype TVar = TV String
    deriving (Eq, Ord, Show)

data Type = TVar TVar
          | TInt | TString | TFloat | TChar
          | TFun Type Type
          | TTuple [Type]
          | TApp Type Type
          | TId String
    deriving (Eq, Ord, Show)

data Kind = KType | KArr Kind Kind | KVar String
    deriving (Eq, Show)

data CustomType = TSum (Map.Map String Scheme) | TProd String Scheme | TAlias Type
    deriving (Eq, Ord, Show)

data CustomScheme = CustomScheme [String] CustomType
    deriving (Eq, Ord, Show)

type TIError = Doc

data Scheme = Scheme [TVar] Type
    deriving (Eq, Ord, Show)

newtype TypeEnv = TypeEnv (Map.Map String Scheme)
    deriving Show

type KindEnv = Map.Map String Kind
type CustomTypeEnv = Map.Map String CustomScheme

type Subst = Map.Map TVar Type

-- | Inference monad
type Infer a = RWST
                  GlobalEnv       -- Typing environment
                  [Constraint]    -- Generated constraints
                  InferState      -- Inference state
                  (Except         -- Inference errors
                    TIError)
                  a               -- Result

-- | Inference state
newtype InferState = InferState { count :: Int }

type Constraint = (Type, Type)

type Unifier = (Subst, [Constraint])

-- | Constraint solver monad
type Solve = Except TIError



data GlobalEnv = GlobalEnv
    { typeDeclCtx :: KindEnv
    , typeDefCtx  :: CustomTypeEnv
    , defCtx      :: TypeEnv
    , ctorCtx     :: TypeEnv }
    deriving Show

type Check = StateT GlobalEnv (Except TIError)

getMap :: TypeEnv -> Map.Map String Scheme
getMap (TypeEnv m) = m

extend :: TypeEnv -> (String, Scheme) -> TypeEnv
extend = TypeEnv .: flip (uncurry Map.insert) . getMap

nullSubst :: Subst
nullSubst = Map.empty

compose :: Subst -> Subst -> Subst
compose s1 s2 = Map.map (apply s1) s2 `Map.union` s1

emptyUnifier :: Unifier
emptyUnifier = (nullSubst, [])

remove :: TypeEnv -> String -> TypeEnv
remove = TypeEnv .: flip Map.delete . getMap

class Substitutable a where
    apply :: Subst -> a -> a
    ftv   :: a -> Set.Set TVar

----------------------------------------------------------------------------------------------

instance Substitutable Type where
    ftv (TVar n)      = Set.singleton n
    ftv TInt          = mempty
    ftv TString       = mempty
    ftv TFloat        = mempty
    ftv TChar         = mempty
    ftv (TFun t1 t2)  = ftv t1 `Set.union` ftv t2
    ftv (TTuple ts)   = List.foldl (\acc t -> acc `Set.union` ftv t) mempty ts
    ftv (TId _)       = mempty
    ftv (TApp t1 t2)  = ftv t1 `Set.union` ftv t2
    ftv _             = mempty

    apply s (TVar n)      = fromMaybe (TVar n) (Map.lookup n s)
    apply s (TFun t1 t2)  = TFun (apply s t1) (apply s t2)
    apply s (TTuple ts)   = TTuple (List.map (apply s) ts)
    apply s (TApp t1 t2)  = TApp (apply s t1) (apply s t2)
    apply _ t             = t

instance Substitutable Scheme where
    ftv (Scheme vars t)     = ftv t Set.\\ Set.fromList vars
    apply s (Scheme vars t) = Scheme vars (apply (foldr Map.delete s vars) t)

instance Substitutable a => Substitutable [a] where
    ftv   = foldr (Set.union . ftv) mempty
    apply = fmap . apply

instance Substitutable TypeEnv where
    ftv   = ftv . Map.elems . getMap
    apply = TypeEnv .: flip (flip (Map.map . apply) . getMap)

instance (Substitutable a, Substitutable b) => Substitutable (a, b) where
    ftv (t1, t2) = ftv t1 `Set.union` ftv t2
    apply s = bimap (apply s) (apply s)


instance Monoid TypeEnv where
    mempty = TypeEnv mempty
    mconcat envs = TypeEnv $ foldl (\acc (TypeEnv t) -> Map.union acc t) mempty envs

instance Semigroup TypeEnv where
    (<>) (TypeEnv env1) (TypeEnv env2) = TypeEnv $ env1 <> env2


instance (Hashable k, Eq k) => AlignWithKey (UMap.Map k)

type instance Key (UMap.Map k) = k

instance (Hashable k, Eq k) => Keyed (UMap.Map k) where
    mapWithKey = UMap.mapWithKey

instance (Hashable k, Eq k) => Align (UMap.Map k) where
    nil = mempty
    align m n = UMap.unionWith merge (UMap.map This m) (UMap.map That n)
      where merge (This a) (That b) = These a b
            merge _ _ = error "Align (UMap.Map k): internal error"