module Blob.TypeChecking.Errors where

import Text.PrettyPrint.Leijen
import Blob.TypeChecking.Types
import Blob.Pretty.Inference

makeUnifyError :: Type -> Type -> TIError
makeUnifyError t1 t2 = text "Could not match type “" <> pType t1 <> text "” with “" <> pType t2 <> text "”" <> dot <> linebreak
makeOccurError :: TVar -> Type -> TIError
makeOccurError (TV s) t1 = text "Occur check fails: cannot construct the infinite type “" <> text s <> text " ~ " <> pType t1 <> text "”" <> dot <> linebreak
makeUnboundVarError :: String -> TIError
makeUnboundVarError s = text "Unbound symbol “" <> text s <> text "”" <> dot <> linebreak
makeRedeclaredError :: String -> TIError
makeRedeclaredError id' = text "Symbol “" <> text id' <> text "” already declared" <> dot <> linebreak
makeRedefinedError :: String -> TIError
makeRedefinedError id' = text "Symbol “" <> text id' <> text "” already defined" <> dot <> linebreak
makeBindLackError :: String -> TIError
makeBindLackError id' = text "“" <> text id' <> text "” lacks an accompanying definition" <> dot <> linebreak