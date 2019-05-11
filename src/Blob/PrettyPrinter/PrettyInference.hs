module Blob.PrettyPrinter.PrettyInference
( pType
, pKind
) where

import Text.PrettyPrint.Leijen (text, parens, Doc, brackets)
import Blob.Inference.Types (Type(..), Kind(..))
import Data.List (intersperse)

pType :: Type -> Doc
pType (TVar id')      = text id'
pType TString         = text "String"
pType TInt            = text "Integer"
pType TFloat          = text "Float"
pType (TFun t1 t2)    = text "(" <> pType t1 <> text " → " <> pType t2 <> text ")"
pType (TRigidVar id') = text id'
pType (TTuple ts)     = parens (mconcat . intersperse (text ", ") $ map pType ts)
pType (TList t)       = brackets $ pType t
pType (TApp t1 t2)    = pType t1 <> text " " <> pType t2
pType (TId u)         = text u

pKind (KVar id')      = text id'
pKind KType           = text "*"
pKind (KArr k1 k2)    = text "(" <> pKind k1 <> text " → " <> pKind k2 <> text ")"
