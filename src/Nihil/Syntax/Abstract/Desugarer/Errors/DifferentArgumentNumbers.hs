module Nihil.Syntax.Abstract.Desugarer.Errors.DifferentArgumentNumbers where

import Nihil.Syntax.Pretty()
import Nihil.Utils.Source
import Nihil.CommonError
import Text.PrettyPrint.ANSI.Leijen
import Prelude hiding ((<$>))
import Control.Arrow ((&&&))
import Data.Functor ((<&>))

differentNumberOfArguments :: String -> Int -> [SourcePos] -> Located Int -> Diagnostic
differentNumberOfArguments name expected pos actual =
    let (loc, fun)        = (location &&& annotated) actual
        labelsForExpected = pos <&> \p -> secondaryLabel p `withLabelMessage` ("Found " <> show expected <> " argument" <> if expected == 1 then "" else "s")
    in errorDiagnostic
           `withMessage` ("Equations for " <> name <> " have varying number of arguments")
           `withBatches` [ newBatch
               `withLabels` (labelsForExpected <>
                             [ secondaryLabel loc `withLabelMessage` ("But expected " <> show expected <> " argument" <> if expected == 1 then "" else "s")
                             , primaryLabel loc `withLabelMessage` ("Got " <> show fun <> " argument" <> if fun == 1 then "" else "s") ] ) ]
