{-# LANGUAGE LambdaCase #-}

module Blob.Pretty.Inference
( pType
, pKind
) where

import Text.PrettyPrint.Leijen (text, parens, Doc, brackets, empty, (<+>))
import Blob.TypeChecking.Types (TVar(..), Type(..), Kind(..))
import Data.List (intersperse)

pType :: Type -> Doc
pType = \case
    TVar (TV i) -> text i
    TId i -> text i
    TInt -> text "Integer"
    TFloat -> text "Double"
    TChar -> text "Char"
    TTuple ts -> parens . mconcat $ intersperse (text ", ") (map pType ts)
    TFun t1 t2 -> parenthesizeIfNeededF t1 <+> text "→" <+> parenthesizeIfNeededF t2
    TApp t1 t2 -> pType t1 <+> parenthesizeIfNeeded t2
  where parenthesizeIfNeeded t = case t of
            TFun _ _ -> parens $ pType t
            TApp _ _ -> parens $ pType t
            _ -> pType t
        parenthesizeIfNeededF t = case t of
            TFun _ _ -> parens $ pType t
            _ -> pType t

pKind :: Kind -> Doc
pKind (KVar id')      = text id'
pKind KType           = text "*"
pKind (KArr k1 k2)    = parenthesizeIfNeeded k1 <+> text "→" <+> parenthesizeIfNeeded k2
  where parenthesizeIfNeeded k = case k of
            KArr _ _ -> parens $ pKind k
            _ -> pKind k
