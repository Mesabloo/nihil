-- Blobc, a compiler for compiling Blob source code
-- Copyright (c) 2019 Mesabloo

-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.

-- You should have received a copy of the GNU General Public License
-- along with this program. If not, see <http://www.gnu.org/licenses/>.

{-# LANGUAGE TypeFamilies, TypeSynonymInstances, TemplateHaskell #-}

-- | This module holds all the types used for the type checking.
module Blob.Language.TypeChecking.Types where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List
import Text.PrettyPrint.Leijen (Doc)
import Control.Monad.Except (Except, ExceptT)
import Control.Monad.Reader (Reader)
import Control.Monad.State (StateT)
import Data.Maybe (fromMaybe)
import Data.Composition ((.:))
import Control.Monad.RWS
import Data.Bifunctor
import Control.Lens

-- | A simple wrapper for a type variable.
newtype TVar = TV String
    deriving (Eq, Ord, Show)

-- | A data type for representing various types, such as:
data Type
    = TVar TVar       -- ^ A substitutable type variable
    | TRigid TVar     -- ^ A rigid (non-substitutable) type variable
    | TInt            -- ^ The integer type (which is an alias defined in 'Blob.Prelude.defaultTypeDefContext')
    | TFloat          -- ^ The float type (also an alias defined in 'Blob.Prelude.defaultTypeDefContext')
    | TChar           -- ^ The char type (also an alias defined in 'Blob.Prelude.defaultTypeDefContext')
    | TFun Type Type  -- ^ A linear function
    | TTuple [Type]   -- ^ A tuple
    | TApp Type Type  -- ^ A type application
    | TId String      -- ^ A type identifier
    | TBang Type      -- ^ A non-linear type
    deriving (Eq, Ord, Show)

-- | A data type to represent the possible kinds in the language.
data Kind
    = KType           -- ^ A simple kind @*@
    | KArr Kind Kind  -- ^ A kind function @k1 -> k2@
    | KVar String     -- ^ A kind variable
    deriving (Eq, Show)
infixr 3 `KArr`

-- | A holder for a custom type.
data CustomType
    = TSum (Map.Map String Scheme)  -- ^ For a sum type, holds the type of each constructor
    | TAlias Type                   -- ^ For a type alias, holds the type of the aliased type
    deriving (Eq, Ord, Show)

-- | A holder for the scheme of a 'CustomType'.
data CustomScheme
    = CustomScheme [String] CustomType
    deriving (Eq, Ord, Show)

-- | The error type
type TIError = Doc

-- | A simple data type for the schme of a 'Type'
--
-- A scheme is composed of a 'Type' and its associated 'TVar's. It can be compared with @forall@.
data Scheme = Scheme [TVar] Type
    deriving (Eq, Ord, Show)

-- | The wrapper for a 'Type' environment.
--
-- It holds a mapping of variables with their 'Scheme's.
newtype TypeEnv = TypeEnv (Map.Map String Scheme)
    deriving Show

-- | A type alias for a 'Kind' environment. Same as 'TypeEnv' but holds 'Kind's instead.
type KindEnv = Map.Map String Kind

-- | A type alias for a 'Customtype' environment. Same as the 'TypeEnv' but holds 'CustomScheme's instead.
type CustomTypeEnv = Map.Map String CustomScheme

-- | An easy way to represent a substitution is with a 'Map.Map'.
--
-- A substitution is basically a way of mapping some type variables to some types.
type Subst = Map.Map TVar Type

-- | The inference monad
type Infer a = RWST
                  GlobalEnv       -- ^ Thet yping environment
                  [Constraint]    -- ^ The generated constraints
                  InferState      -- ^ The inference state
                  (Except         -- ^ The inference errors
                    TIError)
                  a               -- ^ The result of the computation

-- | The state used in the 'Infer' monad.
data InferState
    = InferState { _count :: Int                             -- ^ A counter for generating new type variables
                 , _linearities :: Map.Map String Linearity  -- ^ A mapping from the variable names to their respective 'Linearity'
                 }

-- | The available linearities.
data Linearity
    = Unrestricted  -- ^ Non linear
    | Linear        -- ^ Linear but unused
    | Used          -- ^ Linear and used
    deriving (Show, Eq)

-- | A type constraint.
--
-- It is basically equivalent to saying which types to unify.
type Constraint = (Type, Type)

-- | The unification type.
--
-- It holds a substitution as well as some constraints for unifying with a custom substitution.
type Unifier = (Subst, [Constraint])

-- | The constraint solver monad
type Solve = ExceptT TIError (Reader GlobalEnv)


-- | The typing environment used. It holds:
data GlobalEnv = GlobalEnv
    { _typeDeclCtx :: KindEnv        -- ^ A 'Kind' environment for type correctness checking
    , _typeDefCtx  :: CustomTypeEnv  -- ^ A 'CustomType' environment for data constructor existence checking
    , _defCtx      :: TypeEnv        -- ^ A 'Type' environment for type checking and function definition checking
    , _ctorCtx     :: TypeEnv        -- ^ A 'Type' environment for type checking and data constructor checking
    }
    deriving Show

makeLenses ''InferState
makeLenses ''GlobalEnv

-- | The type checking monad
type Check = StateT GlobalEnv (Except TIError)

-- | Unwraps the underlying 'Map.Map' from a 'TypeEnv'.
getMap :: TypeEnv -> Map.Map String Scheme
getMap (TypeEnv m) = m

-- | Extends a given 'TypeEnv' with a new function.
extend :: TypeEnv -> (String, Scheme) -> TypeEnv
extend = TypeEnv .: flip (uncurry Map.insert) . getMap

-- | The empty substitution.
nullSubst :: Subst
nullSubst = mempty

-- | A special way of composing substitutions together.
compose :: Subst -> Subst -> Subst
compose s1 s2 = Map.map (apply s1) s2 `Map.union` s1

-- | The empty unifier (no substitution, no constraint).
emptyUnifier :: Unifier
emptyUnifier = (nullSubst, [])

-- | Removes an entry from the 'TypeEnv' given.
remove :: TypeEnv -> String -> TypeEnv
remove = TypeEnv .: flip Map.delete . getMap

-- | A type class for the substitutable types.
class Substitutable a where
    -- | Applies a substitution to the given type.
    apply :: Subst -> a -> a
    -- | Gets the free type variables (those which can be substituted) from the given type.
    ftv   :: a -> Set.Set TVar

----------------------------------------------------------------------------------------------

instance Substitutable Type where
    ftv (TVar n)      = Set.singleton n
    ftv (TFun t1 t2)  = ftv t1 `Set.union` ftv t2
    ftv (TTuple ts)   = List.foldl (\acc t -> acc `Set.union` ftv t) mempty ts
    ftv (TApp t1 t2)  = ftv t1 `Set.union` ftv t2
    ftv (TBang t1)    = ftv t1
    ftv _             = mempty

    apply s (TVar n)      = fromMaybe (TVar n) (Map.lookup n s)
    apply s (TFun t1 t2)  = TFun (apply s t1) (apply s t2)
    apply s (TTuple ts)   = TTuple (List.map (apply s) ts)
    apply s (TApp t1 t2)  = TApp (apply s t1) (apply s t2)
    apply s (TBang t1)    = TBang (apply s t1)
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