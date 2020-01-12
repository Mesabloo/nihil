module Nihil.Syntax.Abstract.Desugarer.ShuntingYard where

import Nihil.Utils.Source
import Nihil.Utils.Impossible
import Nihil.Syntax.Common (Desugarer, OperatorTable)
import Nihil.Syntax.Abstract.Desugarer.Errors.UnexpectedOperator
import qualified Nihil.Syntax.Concrete.Core as CC (Associativity(..))
import Control.Monad.Except (throwError)
import Control.Monad (guard)
import Control.Applicative ((<|>))
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map

type OperatorStack = [(String, SourcePos)]
type ValueStack = []

addOperators :: OperatorStack -> ValueStack a -> (String -> SourcePos -> a -> a -> a) -> Desugarer a
addOperators [] [] _                        = impossible "empty output stack"
addOperators (_:_) [] _                     = impossible "empty output stack"
addOperators [] (x:_) _                     = pure x
addOperators ((o,p):_) [_] _                = throwError (unexpected o p)
addOperators ((o, pos):os) (e1:e2:es) mkApp = addOperators os (mkApp o pos e1 e2 : es) mkApp

handleOperator :: String -> SourcePos -> OperatorStack -> ValueStack a -> OperatorTable -> (String -> SourcePos -> a -> a -> a) -> Desugarer (OperatorStack, ValueStack a)
handleOperator o pos [] out _ _                    = pure ([(o, pos)], out)
handleOperator o pos ops@((o1,p):os) es fixs mkApp = do
    let (assoc1, prec1) =
            fromMaybe (impossible "operator not found") (Map.lookup o1 fixs)
        (_, prec2)      =
            fromMaybe (impossible "operator not found") (Map.lookup o fixs)
    if prec1 > prec2 || (prec1 == prec2 && assoc1 == CC.L)
    then do
        guard (length es >= 2)
            <|> throwError (unexpected o1 p)
        let e1:e2:es' = es
        handleOperator o pos os (mkApp o1 p e1 e2 : es') fixs mkApp
    else pure ((o, pos) : ops, es)