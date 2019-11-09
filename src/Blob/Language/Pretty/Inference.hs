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

{-# LANGUAGE LambdaCase #-}

-- | This module contains all the functions related to pretty printing of types and kinds.
module Blob.Language.Pretty.Inference
( pType
, pKind
) where

import Text.PrettyPrint.Leijen (text, parens, Doc, (<+>))
import Blob.Language.TypeChecking.Types (TVar(..), Type(..), Kind(..))
import Data.List (intersperse)
import Blob.Language.Parsing.Annotation

-- | Type pretty printing
pType :: Annotated Type -> Doc
pType (t :- _) = case t of
    TVar (TV i) -> text i
    TRigid (TV i) -> text i
    TId i -> text i
    TInt -> text "Integer"
    TFloat -> text "Double"
    TChar -> text "Char"
    TTuple ts -> parens . mconcat $ intersperse (text ", ") (map (pType . flip (:-) Nothing) ts)
    TFun (t1, l) t2 -> parenthesizeIfNeeded t1 <> text "|" <> text (show l) <> text "|" <+> text "->" <+> pType (t2 :- Nothing)
    TApp t1 t2 -> pType (t1 :- Nothing) <+> parenthesizeIfNeeded t2
  where parenthesizeIfNeeded t = case t of
            TFun _ _ -> parens $ pType (t :- Nothing)
            TApp _ _ -> parens $ pType (t :- Nothing)
            _ -> pType (t :- Nothing)

-- | Kind pretty printing
pKind :: Kind -> Doc
pKind (KVar id')      = text id'
pKind KType           = text "*"
pKind (KArr k1 k2)    = parenthesizeIfNeeded k1 <+> text "->" <+> pKind k2
  where parenthesizeIfNeeded k = case k of
            KArr _ _ -> parens $ pKind k
            _ -> pKind k
