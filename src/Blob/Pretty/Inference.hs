module Blob.Pretty.Inference
( pType
, pKind
) where

import Text.PrettyPrint.Leijen (text, parens, Doc, brackets, empty)
import Blob.TypeChecking.Types (TVar(..), Type(..), Kind(..))
import Data.List (intersperse)

pType :: Type -> Doc
pType (TVar (TV id')) = text id'
pType TString         = text "String"
pType TInt            = text "Integer"
pType TFloat          = text "Float"
pType TChar           = text "Char"
pType (TFun t1 t2)    = text "(" <> pType t1 <> text " → " <> pType t2 <> text ")"
pType (TTuple ts)     = parens (mconcat . intersperse (text ", ") $ map pType ts)
pType (TApp t1 t2)    = pType t1 <> text " " <> pType t2
pType (TId u)         = text u

pKind :: Kind -> Doc
pKind (KVar id')      = text id'
pKind KType           = text "*"
pKind (KArr k1 k2)    = text "(" <> pKind k1 <> text " → " <> pKind k2 <> text ")"
