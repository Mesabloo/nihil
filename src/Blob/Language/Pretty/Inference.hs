{-# LANGUAGE LambdaCase #-}

module Blob.Language.Pretty.Inference
( pType
, pKind
) where

import Text.PrettyPrint.Leijen (text, parens, Doc, brackets, empty, (<+>))
import Blob.Language.TypeChecking.Types (TVar(..), Type(..), Kind(..))
import Data.List (intersperse)
import Blob.Language.Parsing.Annotation

pType :: Annotated Type -> Doc
pType (t :- _) = case t of
    TVar (TV i) -> text i
    TRigid (TV i) -> text i
    TId i -> text i
    TInt -> text "Integer"
    TFloat -> text "Double"
    TChar -> text "Char"
    TTuple ts -> parens . mconcat $ intersperse (text ", ") (map (pType . flip (:-) Nothing) ts)
    TFun t1 t2 -> parenthesizeIfNeededF t1 <+> text "→" <+> pType (t2 :- Nothing)
    TApp t1 t2 -> pType (t1 :- Nothing) <+> parenthesizeIfNeeded t2
  where parenthesizeIfNeeded t = case t of
            TFun _ _ -> parens $ pType (t :- Nothing)
            TApp _ _ -> parens $ pType (t :- Nothing)
            _ -> pType (t :- Nothing)
        parenthesizeIfNeededF t = case t of
            TFun _ _ -> parens $ pType (t :- Nothing)
            _ -> pType (t :- Nothing)

pKind :: Kind -> Doc
pKind (KVar id')      = text id'
pKind KType           = text "*"
pKind (KArr k1 k2)    = parenthesizeIfNeeded k1 <+> text "→" <+> pKind k2
  where parenthesizeIfNeeded k = case k of
            KArr _ _ -> parens $ pKind k
            _ -> pKind k
