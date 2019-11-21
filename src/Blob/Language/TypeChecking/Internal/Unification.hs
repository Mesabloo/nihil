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

{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts #-}

module Blob.Language.TypeChecking.Internal.Unification where

import Blob.Language.TypeChecking.Internal.Errors.Unification
import Blob.Language.PrettyPrinting.Pretty (Pretty)
import Control.Monad.Except (MonadError, throwError)
import Text.PrettyPrint.Leijen (Doc)

class (Monad m, MonadError Doc m, Monoid subst, Pretty a) => Unifiable a subst m where
    unify :: a -> a -> m subst

    unifyMany :: [a] -> [a] -> m subst
    unifyMany [] [] = pure mempty
    unifyMany (t1:ts1) (t2:ts2) = do
        s1 <- unify t1 t2
        s2 <- unifyMany ts1 ts2
        pure (s1 <> s2)
    unifyMany t1 t2 = throwError $ foldMap (uncurry makeUnifyError) (zip t1 t2)