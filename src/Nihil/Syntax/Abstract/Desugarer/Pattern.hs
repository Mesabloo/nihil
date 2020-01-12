module Nihil.Syntax.Abstract.Desugarer.Pattern
( desugarPattern ) where

import qualified Nihil.Syntax.Concrete.Core as CC
import qualified Nihil.Syntax.Abstract.Core as AC
import Nihil.Utils.Source
import Nihil.Syntax.Common (Desugarer, patternLevelOperators)
import Nihil.Utils.Impossible (impossible)
import Nihil.Syntax.Abstract.Desugarer.Type (desugarType)
import Nihil.Syntax.Abstract.Desugarer.ShuntingYard
import Control.Arrow ((&&&))
import Control.Lens (use)

{-| Desugars a pattern (used when pattern matching).

    Most patterns are left untouched, but here is a quick recapitulation of the changes:

    * Infix operators become constructor patterns
      @x \`Cons\` xs@ becomres @Cons x xs@.

    * String literals are transformed into character lists.
      The empty string is however type annotated with @List char@ to not break type checking.

    * Parenthesized patterns are transformed into regular patterns.
-}
desugarPattern :: [CC.APattern] -> Desugarer AC.Pattern
desugarPattern pat = shuntingYard pat [] []

shuntingYard :: [CC.APattern] -> OperatorStack -> ValueStack AC.Pattern -> Desugarer AC.Pattern
shuntingYard [] ops []     = impossible "incomplete output stack"
shuntingYard [] ops out    = addOperators ops out mkApp
shuntingYard (p:ps) ops out = do
    (ops', out') <- uncurry (desugarAtom ops out) ((annotated &&& location) p)
    shuntingYard ps ops' out'

mkApp :: String -> SourcePos -> AC.Pattern -> AC.Pattern -> AC.Pattern
mkApp operator pos p1 p2 =
    locate (AC.PConstructor operator [p2, p1]) pos

desugarAtom :: OperatorStack -> ValueStack AC.Pattern -> CC.Pattern -> SourcePos -> Desugarer (OperatorStack, ValueStack AC.Pattern)
desugarAtom ops out (CC.POperator o) pos              = do
    fixs <- use patternLevelOperators
    handleOperator o pos ops out fixs mkApp
desugarAtom ops out (CC.PId i) pos                    = pure (ops, locate (AC.PId i) pos : out)
desugarAtom ops out CC.PWildcard pos                  = pure (ops, locate AC.PWildcard pos : out)
desugarAtom ops out (CC.PLiteral (CC.LString "")) pos =
    let eStrTy = locate (AC.TApplication (locate (AC.TId "List") pos) (locate (AC.TId "Char") pos)) pos
        pAnn   = locate (AC.PTypeAnnotated (locate (AC.PId "Nil") pos) eStrTy) pos
    in pure (ops, pAnn : out)
desugarAtom ops out (CC.PLiteral (CC.LString s)) pos  =
    let expr = foldr (mkApp' "Cons" pos) (locate (AC.PId "Nil") pos) s
    in pure (ops, expr : out)
  where mkApp' cons pos c acc = mkApp cons pos acc (locate (AC.PLiteral (AC.LCharacter c)) pos)
desugarAtom ops out (CC.PLiteral lit) pos             = pure (ops, locate (AC.PLiteral (desugarLiteral lit)) pos : out)
  where desugarLiteral (CC.LInteger i)   = AC.LInteger i
        desugarLiteral (CC.LCharacter c) = AC.LCharacter c
        desugarLiteral (CC.LDouble d)    = AC.LFloat d
        desugarLiteral (CC.LString _)    = impossible "String already desugared"
desugarAtom ops out (CC.PParens pat) pos              = do
    p <- desugarPattern pat
    pure (ops, p : out)
desugarAtom ops out (CC.PTypeAnnotated pat ty) pos    = do
    p <- desugarPattern pat
    t <- desugarType ty
    let pAnn = locate (AC.PTypeAnnotated p t) pos
    pure (ops, pAnn : out)
desugarAtom ops out (CC.PTuple ps) pos                = do
    pats <- mapM desugarPattern ps
    let pTuple = locate (AC.PTuple pats) pos
    pure (ops, pTuple : out)
desugarAtom ops out (CC.PConstructor name pats) pos    = do
    ps <- mapM (\p -> shuntingYard [p] [] []) pats
    let pCtor = locate (AC.PConstructor name ps) pos
    pure (ops, pCtor : out)