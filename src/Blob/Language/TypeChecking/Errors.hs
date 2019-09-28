-- | This module holds all the possible errors for the type-checking.
module Blob.Language.TypeChecking.Errors where

import Text.PrettyPrint.Leijen
import Blob.Language.TypeChecking.Types
import Blob.Language.Pretty.Inference
import Blob.Language.Parsing.Annotation

makeUnifyError :: Type -> Type -> TIError
makeUnifyError t1 t2 =
    text "- Type mismatch" <> linebreak
    <> text "  > Expected type: " <> pType (t1 :- Nothing) <> linebreak
    <> text "  > Actual type:   " <> pType (t2 :- Nothing) <> linebreak

makeOccurError :: TVar -> Type -> TIError
makeOccurError (TV s) t1 =
    text "- Occur check fails" <> linebreak
    <> text "  > Cannot construct an infinite type" <> linebreak
    <> text "  > In type: \"" <> text s <> text " ~ " <> pType (t1 :- Nothing) <> text "\"" <> linebreak

makeUnboundVarError :: String -> TIError
makeUnboundVarError s =
    text "- Unbound symbol \"" <> text s <> text "\"" <> linebreak
    <> text "- Probable causes:" <> linebreak
    <> text "  > The symbol is never defined" <> linebreak
    <> text "  > The symbol is defined in another module that you didn't import" <> linebreak

makeRedeclaredError :: String -> TIError
makeRedeclaredError id' =
    text "- Duplicated type signature" <> linebreak
    <> text "  > For the symbol: " <> text id' <> linebreak

makeRedefinedError :: String -> TIError
makeRedefinedError id' =
    text "- Duplicated symbol definition" <> linebreak
    <> text "  > For the symbol: " <> text id' <> linebreak

makeBindLackError :: String -> TIError
makeBindLackError id' =
    text "- Missing definition" <> linebreak
    <> text "  > For the symbol: " <> text id' <> linebreak

makeHoleError :: Type -> TIError
makeHoleError t1 =
    text "- Found hole:" <> linebreak
    <> text "  > Fitting type: " <> pType (t1 :- Nothing) <> linebreak

makeGADTWrongReturnTypeError :: String -> Type -> Type -> TIError
makeGADTWrongReturnTypeError ctorName actualType expectedType =
    text "- GADT return type mismatch" <> linebreak
    <> text "  > In constructor: " <> text ctorName <> linebreak
    <> text "  > Expected type:  " <> pType (expectedType :- Nothing) <> linebreak
    <> text "  > Actual type:    " <> pType (actualType :- Nothing) <> linebreak

makeTooMuchUsagesError :: String -> TIError
makeTooMuchUsagesError name =
    text ("- The symbol \"" <> name <> "\" has already been used once in the current expression.") <> linebreak
    <> text "- Possible fix:" <> linebreak
    <> text ("  > Make \"" <> name <> "\" non-linear by adding `!` to its type.") <> linebreak