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

module Nihil.Interpreter.Value where

import Nihil.Interpreter.Scope
import Nihil.Interpreter.Evaluator (Eval)
import Language.Nihil.Syntax.Internal.Parsing.Located (Located)
import Language.Nihil.Syntax.Internal.Desugaring.CoreAST
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

-- | The data type providing any sort of value, for example:
data Value
    = VInt Integer                                             -- ^ An integer
    | VDec Double                                              -- ^ A floating point number
    | VChr Char                                                -- ^ A character
    | VVar String                                              -- ^ A variable
    | VLam (Located Pattern) (Located Expr) (Scope Value)      -- ^ A lambda (remains unevaluated until the argument value is given)
    | VTuple [Value]                                           -- ^ A tuple
    | HLam (Value -> Eval Value)                               -- ^ A native lambda (implemented in Haskell)
    | VCon String [Value]                                      -- ^ A data type constructor

instance Pretty Value where
    pretty (VInt i)     = integer i
    pretty (VDec d)     = double d
    pretty (VChr c)     = squotes $ char c
    pretty (VVar id')   = text id'
    pretty (VTuple vs)  = tupled (pretty <$> vs)
    pretty (VCon id' e)
        | null e        = text id'
        | otherwise     =
            text id' <> foldl ((. parenthesizeIfNeeded) . (<+>)) empty e
      where
        parenthesizeIfNeeded v@(VCon id'' e')
            | null e'   = text id''
            | otherwise = parens (pretty v)
        parenthesizeIfNeeded v =
            pretty v
    pretty _ = text "No instance Show for type: a -> b" -- text ""