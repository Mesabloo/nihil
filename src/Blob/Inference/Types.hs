module Blob.Inference.Types where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List
import Text.PrettyPrint.Leijen (Doc)
import Control.Monad.Except (Except, ExceptT)
import Control.Monad.Reader (ReaderT)
import Control.Monad.State (StateT, State)
import Data.Maybe (fromMaybe)

class Types a where
    ftv :: a -> Set.Set String
    apply :: Subst -> a -> a

data Type = TVar String
          | TRigidVar String
          | TInt | TString | TFloat
          | TFun Type Type
          | TTuple [Type]
          | TList Type
    deriving (Eq, Ord, Show)

type TIError = Doc

data Scheme = Scheme [String] Type
    deriving Show

newtype TypeEnv = TypeEnv (Map.Map String Scheme)
    deriving Show

type Subst = Map.Map String Type

data TIState = TIState
    { tiSupply :: Integer
    , tiSubst :: Subst }

type TI a = ExceptT TIError (ReaderT TypeEnv (State TIState)) a

data GlobalEnv = GlobalEnv { declCtx :: TypeEnv, defCtx :: TypeEnv }

type Check a = StateT GlobalEnv (Except TIError) a

----------------------------------------------------------------------------------------------
instance Types Type where
    ftv (TVar n)      = Set.singleton n
    ftv TInt          = mempty
    ftv TString       = mempty
    ftv TFloat        = mempty
    ftv (TFun t1 t2)  = ftv t1 `Set.union` ftv t2
    ftv (TTuple ts)   = List.foldl (\acc t -> acc `Set.union` ftv t) mempty ts 
    ftv (TList t)     = ftv t
    ftv (TRigidVar _) = mempty

    apply s (TVar n)      = fromMaybe (TVar n) (Map.lookup n s)
    apply s (TFun t1 t2)  = TFun (apply s t1) (apply s t2)
    apply s (TRigidVar n) = fromMaybe (TRigidVar n) (Map.lookup n s)
    apply s (TTuple ts)   = TTuple (List.map (apply s) ts)
    apply s (TList t)     = TList (apply s t)
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