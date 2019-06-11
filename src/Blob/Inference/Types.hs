{-# LANGUAGE TypeFamilies, TypeSynonymInstances #-}

module Blob.Inference.Types where

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

class Types a where
    ftv :: a -> Set.Set String
    apply :: Subst -> a -> a

data Type = TVar String
          | TRigidVar String
          | TInt | TString | TFloat
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

data Scheme = Scheme [String] Type
    deriving (Eq, Ord, Show)

newtype TypeEnv = TypeEnv (Map.Map String Scheme)
    deriving Show

type KindEnv = Map.Map String Kind
type CustomTypeEnv = Map.Map String CustomScheme

type Subst = Map.Map String Type

data TIState = TIState
    { tiSupply :: Integer
    , tiSubst :: Subst }

type TI a = ExceptT TIError (ReaderT GlobalEnv (State TIState)) a

data GlobalEnv = GlobalEnv
    { typeDeclCtx :: KindEnv
    , typeDefCtx  :: CustomTypeEnv
    , defCtx      :: TypeEnv
    , ctorCtx     :: TypeEnv }
    deriving Show

type Check a = StateT GlobalEnv (Except TIError) a

nullSubst :: Subst
nullSubst = mempty

composeSubst :: Subst -> Subst -> Subst
composeSubst s1 s2 = Map.map (apply s1) s2 `Map.union` s1

remove :: TypeEnv -> String -> TypeEnv
remove (TypeEnv env) var = TypeEnv (Map.delete var env)

insert :: String -> Scheme -> TypeEnv -> TypeEnv
insert k v (TypeEnv env) = TypeEnv $ Map.insert k v env

insertFun :: String -> Scheme -> GlobalEnv -> GlobalEnv
insertFun k v (GlobalEnv tdc tdf def ctor) = GlobalEnv { typeDeclCtx = tdc
                                                       , typeDefCtx  = tdf
                                                       , defCtx      = insert k v def
                                                       , ctorCtx     = ctor }

insertEnv :: TypeEnv -> GlobalEnv -> GlobalEnv
insertEnv (TypeEnv t) (GlobalEnv tdc tdf (TypeEnv def) ctor) = GlobalEnv { typeDeclCtx = tdc
                                                                         , typeDefCtx  = tdf
                                                                         , defCtx      = TypeEnv $ def `Map.union` t
                                                                         , ctorCtx     = ctor }

lookup' :: TypeEnv -> String -> Maybe Scheme
lookup' (TypeEnv env) n = Map.lookup n env

getScheme :: String -> TypeEnv -> Maybe Scheme
getScheme k (TypeEnv env) = env Map.!? k

----------------------------------------------------------------------------------------------

instance Types Type where
    ftv (TVar n)      = Set.singleton n
    ftv TInt          = mempty
    ftv TString       = mempty
    ftv TFloat        = mempty
    ftv (TFun t1 t2)  = ftv t1 `Set.union` ftv t2
    ftv (TTuple ts)   = List.foldl (\acc t -> acc `Set.union` ftv t) mempty ts
    ftv (TRigidVar _) = mempty
    ftv (TId _)       = mempty
    ftv (TApp t1 t2)  = ftv t1 `Set.union` ftv t2
    ftv _             = mempty

    apply s (TVar n)      = fromMaybe (TVar n) (Map.lookup n s)
    apply s (TFun t1 t2)  = TFun (apply s t1) (apply s t2)
    apply s (TRigidVar n) = fromMaybe (TRigidVar n) (Map.lookup n s)
    apply s (TTuple ts)   = TTuple (List.map (apply s) ts)
    apply s (TApp t1 t2)  = TApp (apply s t1) (apply s t2)
    apply _ t             = t

instance Types Scheme where
    ftv (Scheme vars t)     = ftv t Set.\\ Set.fromList vars
    apply s (Scheme vars t) = Scheme vars (apply (foldr Map.delete s vars) t)

instance Types a => Types [a] where
    apply s = map (apply s)
    ftv     = foldr (Set.union . ftv) mempty

instance Types TypeEnv where
    ftv (TypeEnv env)     = ftv (Map.elems env)
    apply s (TypeEnv env) = TypeEnv (Map.map (apply s) env)


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