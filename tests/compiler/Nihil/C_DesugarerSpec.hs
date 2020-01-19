{-# LANGuaGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Nihil.C_DesugarerSpec (spec) where

import Test.Hspec
import Nihil.Syntax (runLexer, runParser, runDesugarer)
import Nihil.Utils.Source (locate, Located, SourcePos(NoSource))
import Nihil.Syntax.Abstract
import qualified Data.Text as Text (Text)
import qualified Data.Map as Map (fromList)

spec :: Spec
spec = do
    describe "Test on parameter lambda expansion"         parameterLambdaExpansionTest
    describe "Test on operator application expansion"     operatorApplicationExpansionTest
    describe "Test on sumtype to GADT expansion"          sumtypeGADTExpansionTest
    describe "Test on `where` to `let` expansion"         whereLetExpansionTest
    describe "Test on string to character list expansion" stringCharListExpansionTest

parameterLambdaExpansionTest :: Spec
parameterLambdaExpansionTest = do
    testAST "id x = x" "should be correctly expanded to a lambda"
        (Program [node (FunctionDefinition "id" (node (ELambda (node (PId "x")) (node (EId "x")))))])

operatorApplicationExpansionTest :: Spec
operatorApplicationExpansionTest = do
    testAST "f x = x + 1" "should be correctly expanded into a function application"
        (Program [node (FunctionDefinition "f" (node (ELambda (node (PId "x")) (node (EApplication (node (EApplication (node (EId "+")) (node (EId "x")))) (node (ELiteral (LInteger 1))))))))])

sumtypeGADTExpansionTest :: Spec
sumtypeGADTExpansionTest = do
    testAST "data Maybe a = Nothing | Just a" "should be correctly expanded into a GADT"
        (Program [node (TypeDefinition "Maybe" ["a"] (node (SumType (Map.fromList [("Nothing", Forall ["a"] (node (TApplication (node (TId "Maybe")) (node (TVar "a"))))), ("Just", Forall ["a"] (node (TApplication (node (TApplication (node (TId "->")) (node (TVar "a")))) (node (TApplication (node (TId "Maybe")) (node (TVar "a")))))))]))))])

whereLetExpansionTest :: Spec
whereLetExpansionTest = do
    testAST "g = f where f = 0" "should be correctly expanded to a let expression"
        (Program [node (FunctionDefinition "g" (node (ELet [node (FunctionDefinition "f" (node (ELiteral (LInteger 0))))] (node (EId "f")))))])

stringCharListExpansionTest :: Spec
stringCharListExpansionTest = do
    testAST "f = \"xyz\"" "should be correctly expanded to a character list"
        (Program [node (FunctionDefinition "f" (cons 'x' (cons 'y' (cons 'z' (node (EId "Nil"))))))])
  where cons char list = node (EApplication (node (EApplication (node (EId "Cons")) (node (ELiteral (LCharacter char))))) list)

-----------------------------------------------------------------------------------------------------------

node :: a -> Located a
node = (`locate` NoSource)

testAST :: Text.Text -> String -> Program -> Spec
testAST code msg expected = do
    let (Right lex) = runLexer code "test"
    let (Right ast) = runParser lex "test"
    let (Right dst) = runDesugarer ast
    it msg do
        dst `shouldBe` expected