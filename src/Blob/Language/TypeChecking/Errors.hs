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