module Nihil.Syntax.Abstract.Desugarer.Errors.UnexpectedOperator where

import Nihil.Utils.Source
import Nihil.Utils.Debug
import Prelude hiding (error)

{-| [Unexpected operator error]

    Occurs when an operation is misplaced in an expression.
    This can happen when there is either both or one of the operands missing.

    For example:

    > f = 0 +
    > g = -

    Both definitions will yield errors.

    There are multiple ways to solve this problem:

    * Add the missing operand(s) to the operator. That way it will parse completely.

    * Surround the operator with parentheses (or remove the backticks around the function name).
      Perhaps you meant to call it like a function?

    * Delete the erroneous token and try again.
-}
unexpected :: String -> SourcePos -> String
unexpected operator pos =
    error ("unexpected operator " <> operator <> " ; " <> show pos) ("Unexpected operator '" <> operator <> "' at " <> show pos)