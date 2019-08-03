module Blob.Language.TypeChecking.Errors where

import Text.PrettyPrint.Leijen
import Blob.Language.TypeChecking.Types
import Blob.Language.Pretty.Inference
import Blob.Language.Parsing.Annotation

makeUnifyError :: Type -> Type -> TIError
makeUnifyError t1 t2 = text "Could not match type “" <> pType (t1 :- Nothing) <> text "” with “" <> pType (t2 :- Nothing) <> text "”" <> dot <> linebreak
makeOccurError :: TVar -> Type -> TIError
makeOccurError (TV s) t1 = text "Occur check fails: cannot construct the infinite type “" <> text s <> text " ~ " <> pType (t1 :- Nothing) <> text "”" <> dot <> linebreak
makeUnboundVarError :: String -> TIError
makeUnboundVarError s = text "Could not resolve type of symbol “" <> text s <> text "”" <> dot <+> text "Maybe it has not been defined?" <> linebreak
makeRedeclaredError :: String -> TIError
makeRedeclaredError id' = text "Symbol “" <> text id' <> text "” already has an explicit type declaration" <> dot <> linebreak
makeRedefinedError :: String -> TIError
makeRedefinedError id' = text "Symbol “" <> text id' <> text "” already has a definition" <> dot <> linebreak
makeBindLackError :: String -> TIError
makeBindLackError id' = text "Symbol “" <> text id' <> text "” lacks an accompanying definition" <> dot <> linebreak
makeHoleError :: Type -> TIError
makeHoleError t1 = text "Found hole: _ :: " <> pType (t1 :- Nothing) <> dot <> linebreak
makeGADTWrongReturnTypeError :: String -> Type -> Type -> TIError
makeGADTWrongReturnTypeError ctorName actualType expectedType =
    text ("GADT constructor “" <> ctorName <> "” does not return right type: expected type “") <> pType (expectedType :- Nothing)
    <> text "”, got type “" <> pType (actualType :- Nothing) <> text "”" <> dot <> linebreak