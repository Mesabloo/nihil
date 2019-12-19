-- The Great Nihil Compiler
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

{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE TupleSections #-}

module Language.Nihil.Syntax.Rules.Desugaring.Toplevel.CustomType where

import Language.Nihil.Syntax.Internal.Parsing.Located
import qualified Language.Nihil.Syntax.Internal.Parsing.AST as P
import qualified Language.Nihil.Syntax.Internal.Desugaring.CoreAST as D
import Language.Nihil.Syntax.Desugarer (Desugarer)
import Language.Nihil.Syntax.Rules.Desugaring.Types.Type
import qualified Data.Map as Map
import Data.Composition ((.:))

-- Desugars a 'P.CustomType' into a 'D.CustomType'
desugarCustomType :: String -> (String, [String], Located P.CustomType) -> Desugarer (Located D.CustomType)
desugarCustomType fileName (name, tvs, ct :@ p) = case ct of
    P.TAlias t -> do
        t' <- desugarType fileName t
        pure $ D.TAlias t' :@ p
    P.TSum s -> do
        {- How to desugar a sum type:
            * First transform the base type into a type application
            * Then concat this type with the type of the constructor: `consType <> defType`
        -}

        let initialType = foldl (flip (:@) Nothing .: D.TApp) (D.TId name :@ Nothing) (flip (:@) Nothing . D.TVar <$> tvs)
            folded = foldr (flip (:@) Nothing .: D.TFun . (, 1)) initialType
            m = Map.map (\cs -> do
                cs' <- traverse (desugarType fileName) cs
                pure (D.Scheme tvs $ folded cs')) s
        m' <- sequence m

        pure (D.TSum m' :@ p)
    P.TGADT s -> do -- Desugaring a 'P.TGADT' is equivalent to transforming it into a 'D.TSum'
        let m = Map.map (\c -> do
                c' <- desugarType fileName c
                pure (D.Scheme tvs c')) s
        m' <- sequence m

        pure (D.TSum m' :@ p)