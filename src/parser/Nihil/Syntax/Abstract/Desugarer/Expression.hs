{-# LANGUAGE BlockArguments #-}

module Nihil.Syntax.Abstract.Desugarer.Expression
( desugarExpression ) where

import qualified Nihil.Syntax.Concrete.Core as CC
import qualified Nihil.Syntax.Abstract.Core as AC
import Nihil.Utils.Source
import Nihil.Syntax.Common (Desugarer, valueLevelOperators)
import Nihil.Utils.Debug
import Nihil.Utils.Impossible (impossible)
import Nihil.Syntax.Abstract.Desugarer.Type (desugarType)
import Nihil.Syntax.Abstract.Desugarer.Pattern (desugarPattern)
import Nihil.Syntax.Abstract.Desugarer.Statement (desugarProgram)
import Nihil.Syntax.Abstract.Desugarer.ShuntingYard
import Nihil.Syntax.Abstract.Desugarer.Errors.UnexpectedOperator
import Control.Arrow ((&&&))
import Control.Lens (use)
import Control.Monad.Except (throwError)

{-| Desugars an expression.

    Most expressions are just converted, while some others are actually desugared.

    Here is a quick recapitulation:

    * String literals are transformed into character lists.
      Empty strings are however annotated with a type to keep type safety (because @"" : List Char@).

    * @where@ expressions are transformed into @let@ expressions.

    * Parenthesized expressions are simply converted to basic expressions.

    * Infix operators are converted to function applications.

    * Multi parameters lambda abstractions are converted to multiple lambda abstractions.
      @λ x y → e@ becomes @λ x → λ y → e@.
-}
desugarExpression :: CC.Expr -> Desugarer AC.Expr
desugarExpression expr = shuntingYard expr [] []

shuntingYard :: [CC.AAtom] -> OperatorStack -> ValueStack AC.Expr -> Desugarer AC.Expr
shuntingYard [] ((o,p):_) [] = throwError (unexpected o p)
shuntingYard [] ops out      = addOperators ops out mkApp
shuntingYard (e:es) ops out  = do
    (ops', out') <- uncurry (desugarAtom ops out) ((annotated &&& location) e)
    shuntingYard es ops' out'

mkApp :: String -> SourcePos -> AC.Expr -> AC.Expr -> AC.Expr
mkApp operator pos e1 e2 =
    locate (AC.EApplication (locate (AC.EApplication (locate (AC.EId operator) pos) e2) pos) e1) pos

desugarAtom :: OperatorStack -> ValueStack AC.Expr -> CC.Atom -> SourcePos -> Desugarer (OperatorStack, ValueStack AC.Expr)
desugarAtom ops out (CC.AOperator o) pos              = do
    fixs <- use valueLevelOperators
    handleOperator o pos ops out fixs mkApp
desugarAtom ops out CC.ATypeHole pos                  = pure (ops, locate AC.ETypeHole pos : out)
desugarAtom ops out (CC.AId i) pos                    = pure (ops, locate (AC.EId i) pos : out)
desugarAtom ops out (CC.ALiteral (CC.LString "")) pos =
    let eStrTy = locate (AC.TApplication (locate (AC.TId "List") pos) (locate (AC.TId "Char") pos)) pos
        eAnn   = locate (AC.ETypeAnnotated (locate (AC.EId "Nil") pos) eStrTy) pos
    in pure (ops, eAnn : out)
desugarAtom ops out (CC.ALiteral (CC.LString s)) pos  =
    let expr = foldr (mkApp' "Cons" pos) (locate (AC.EId "Nil") pos) s
    in pure (ops, expr : out)
  where mkApp' cons pos c acc = mkApp cons pos acc (locate (AC.ELiteral (AC.LCharacter c)) pos)
desugarAtom ops out (CC.ALiteral lit) pos            = pure (ops, locate (AC.ELiteral (desugarLiteral lit)) pos : out)
  where desugarLiteral (CC.LCharacter c) = AC.LCharacter c
        desugarLiteral (CC.LInteger i)   = AC.LInteger i
        desugarLiteral (CC.LDouble d)    = AC.LFloat d
        desugarLiteral (CC.LString _)    = impossible "String already desugared"
desugarAtom ops out (CC.AParens e) pos                = do
    expr <- desugarExpression e
    pure (ops, expr : out)
desugarAtom ops out (CC.AWhere e ss) pos              = desugarAtom ops out (CC.ALet ss e) pos
desugarAtom ops out (CC.AApplication atoms) pos       = do
    exprs <- mapM (\a -> shuntingYard [a] [] []) atoms
    errorWhen (null exprs) "No expression found for an application: parsing failed" (pure ())
    let eApp = foldl1 (mkApp' pos) exprs
    pure (ops, eApp : out)
  where mkApp' pos e1 e2 = locate (AC.EApplication e1 e2) pos
desugarAtom ops out (CC.ALambda patterns expr) pos    = do
    pats <- mapM (\p -> desugarPattern [p]) patterns
    e    <- desugarExpression expr
    errorWhen (null pats) "No patterns found for a lambda: parsing failed" (pure ())
    let eLam = foldr (mkLam pos) e pats
    pure (ops, eLam : out)
  where mkLam pos p1 e2 = locate (AC.ELambda p1 e2) pos
desugarAtom ops out (CC.ATuple es) pos                = do
    exprs <- mapM desugarExpression es
    pure (ops, locate (AC.ETuple exprs) pos : out)
desugarAtom ops out (CC.ATypeAnnotated expr ty) pos   = do
    e <- desugarExpression expr
    t <- desugarType ty
    let eAnn = locate (AC.ETypeAnnotated e t) pos
    pure (ops, eAnn : out)
desugarAtom ops out (CC.ALet ss e) pos                = do
    expr <- desugarExpression e
    AC.Program stts <- desugarProgram (CC.Program ss)
    let eLet = locate (AC.ELet stts expr) pos
    pure (ops, eLet : out)
desugarAtom ops out (CC.AMatch e1 branches) pos         = do
    ex    <- desugarExpression e1
    match <- AC.EMatch ex <$> desugarBranches branches
    pure (ops, locate match pos : out)
  where desugarBranches :: [([CC.APattern], CC.Expr)] -> Desugarer [(AC.Pattern, AC.Expr)]
        desugarBranches = mapM \(p, e) -> do
            pat <- desugarPattern p
            ex  <- desugarExpression e
            pure (pat, ex)
