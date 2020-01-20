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
spec = parallel do
    describe "Tests on expressions" do
        describe "Test on empty string"              expressionEmptyStringExpansionTest
        describe "Test on literals"                  expressionLiteralsExpansionTest
        describe "Test on operators"                 expressionOperatorExpansionTest
        describe "Test on type holes"                expressionTypeholeExpansionTest
        describe "Test on parenthesized expression"  expressionParensExpansionTest
        describe "Test on expression application"    expressionApplicationExpansionTest
        describe "Test on tuples"                    expressionTupleExpansionTest
        describe "Test on type annotated expression" expressionTypeAnnotatedExpansionTest
    describe "Tests on types" do
        describe "Test on unit type"         typeUnitExpansionTest
        describe "Test on type variable"     typeVariableExpansionTest
        describe "Test on type identifier"   typeIdentifierExpansionTest
        describe "Test on type operators"    typeOperatorsExpansionTest
        describe "Test on type application"  typeApplicationExpansionTest
    describe "Tests on patterns" do
        describe "Test on wildcard pattern"       patternWildcardExpansionTest
        describe "Test on identifier pattern"     patternIdentifierExpansionTest
        describe "Test on pattern literal"        patternLiteralExpansionTest
        describe "Test on pattern tuple"          patternTupleExpansionTest
        describe "Test on type annotated pattern" patternTypeAnnotatedExpansionTest
        describe "Test on constructor pattern"    patternConstructorExpansionTest
        describe "Test on pattern operators"      patternOperatorsExpansionTest
    describe "Special tests" do
        describe "Test on parameter lambda expansion"         parameterLambdaExpansionTest
        describe "Test on operator application expansion"     operatorApplicationExpansionTest
        describe "Test on sumtype to GADT expansion"          sumtypeGADTExpansionTest
        describe "Test on `where` to `let` expansion"         whereLetExpansionTest
        describe "Test on string to character list expansion" stringCharListExpansionTest

parameterLambdaExpansionTest :: Spec
parameterLambdaExpansionTest = do
    testAST "id x = x" "should be correctly expanded to a lambda"
        (Program [node (FunctionDefinition "id" (node (ELambda (node (PId "%0")) (node (EMatch (node (ETuple [node (EId "%0")])) [(node (PTuple [node (PId "x")]), node (EId "x"))])))))])

operatorApplicationExpansionTest :: Spec
operatorApplicationExpansionTest = do
    testAST "f x = x + 1" "should be correctly expanded into a function application"
        (Program [node (FunctionDefinition "f" (node (ELambda (node (PId "%0")) (node (EMatch (node (ETuple [node (EId "%0")])) [(node (PTuple [node (PId "x")]), (node (EApplication (node (EApplication (node (EId "+")) (node (EId "x")))) (node (ELiteral (LInteger 1))))))])))))])

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



expressionEmptyStringExpansionTest :: Spec
expressionEmptyStringExpansionTest = do
    testAST "empty = \"\"" "should be correctly expanded into a type annotated list"
        (Program [node (FunctionDefinition "empty" (node (ETypeAnnotated (node (EId "Nil")) (node (TApplication (node (TId "List")) (node (TId "Char")))))))])

expressionLiteralsExpansionTest :: Spec
expressionLiteralsExpansionTest = do
    testAST "char = 'c'" "should be correctly expanded to a character literal"
        (Program [node (FunctionDefinition "char" (node (ELiteral (LCharacter 'c'))))])
    testAST "int = 3" "should be correctly expanded to an integer literal"
        (Program [node (FunctionDefinition "int" (node (ELiteral (LInteger 3))))])
    testAST "double = 0.32e+3" "should be correctly expanded to a float literal"
        (Program [node (FunctionDefinition "double" (node (ELiteral (LFloat 0.32e+3))))])

expressionOperatorExpansionTest :: Spec
expressionOperatorExpansionTest = do
    testAST "infixl 2 %\nw = 1 + 1 % 3" "should be correctly expanded to function applications"
        (Program [node (FunctionDefinition "w" (eApp (eApp (node (EId "%")) (eApp (eApp (node (EId "+")) (node (ELiteral (LInteger 1)))) (node (ELiteral (LInteger 1))))) (node (ELiteral (LInteger 3)))))])
  where eApp e1 e2 = node (EApplication e1 e2)

expressionTypeholeExpansionTest :: Spec
expressionTypeholeExpansionTest = do
    testAST "f = _" "should be correctly expanded to a type hole"
        (Program [node (FunctionDefinition "f" (node ETypeHole))])

expressionParensExpansionTest :: Spec
expressionParensExpansionTest = do
    testAST "f = (0 + 1)" "should be correctly expanded into a normal expression"
        (Program [node (FunctionDefinition "f" (node (EApplication (node (EApplication (node (EId "+")) (node (ELiteral (LInteger 0))))) (node (ELiteral (LInteger 1))))))])

expressionApplicationExpansionTest :: Spec
expressionApplicationExpansionTest = do
    testAST "f = id 0" "should be correctly expanded into an expression application"
        (Program [node (FunctionDefinition "f" (node (EApplication (node (EId "id")) (node (ELiteral (LInteger 0))))))])

expressionTupleExpansionTest :: Spec
expressionTupleExpansionTest = do
    testAST "x = (a, b)" "should be correctly expanded to a tuple"
        (Program [node (FunctionDefinition "x" (node (ETuple [node (EId "a"), node (EId "b")])))])

expressionTypeAnnotatedExpansionTest :: Spec
expressionTypeAnnotatedExpansionTest = do
    testAST "w = 0 : Integer" "should be correctly expanded into a type annotated expression"
        (Program [node (FunctionDefinition "w" (node (ETypeAnnotated (node (ELiteral (LInteger 0))) (node (TId "Integer")))))])



typeUnitExpansionTest :: Spec
typeUnitExpansionTest = do
    testAST "f: ()" "should be correctly expanded to an empty tuple"
        (Program [node (FunctionDeclaration "f" (node (TTuple [])))])

typeVariableExpansionTest :: Spec
typeVariableExpansionTest = do
    testAST "f: a" "should be correctly parsed to a type variable"
        (Program [node (FunctionDeclaration "f" (node (TVar "a")))])

typeIdentifierExpansionTest :: Spec
typeIdentifierExpansionTest = do
    testAST "f: Integer" "should be correctly parsed into a type identifier"
        (Program [node (FunctionDeclaration "f" (node (TId "Integer")))])

typeOperatorsExpansionTest :: Spec
typeOperatorsExpansionTest = do
    testAST "f: x -> y" "should be correctly expanded to a function application"
        (Program [node (FunctionDeclaration "f" (node (TApplication (node (TApplication (node (TId "->")) (node (TVar "x")))) (node (TVar "y")))))])

typeApplicationExpansionTest :: Spec
typeApplicationExpansionTest = do
    testAST "f: Maybe a" "should be correctly expanded to a function application"
        (Program [node (FunctionDeclaration "f" (node (TApplication (node (TId "Maybe")) (node (TVar "a")))))])



patternWildcardExpansionTest :: Spec
patternWildcardExpansionTest = do
    testAST "f _ = 0" "should be correctly expanded to a wildcard"
        (Program [node (FunctionDefinition "f" (node (ELambda (node (PId "%0")) (node (EMatch (node (ETuple [node (EId "%0")])) [(node (PTuple [node PWildcard]), node (ELiteral (LInteger 0)))])))))])

patternIdentifierExpansionTest :: Spec
patternIdentifierExpansionTest = do
    testAST "f x = x" "should be correctly expanded to an identifier"
        (Program [node (FunctionDefinition "f" (node (ELambda (node (PId "%0")) (node (EMatch (node (ETuple [node (EId "%0")])) [(node (PTuple [node (PId "x")]), node (EId "x"))])))))])

patternLiteralExpansionTest :: Spec
patternLiteralExpansionTest = do
    testAST "f 0 = x" "should be correctly expanded into a literal"
        (Program [node (FunctionDefinition "f" (node (ELambda (node (PId "%0")) (node (EMatch (node (ETuple [node (EId "%0")])) [(node (PTuple [node (PLiteral (LInteger 0))]), node (EId "x"))])))))])

patternTupleExpansionTest :: Spec
patternTupleExpansionTest = do
    testAST "f (a, 0.3) = a" "should be correctly expanded into a tuple"
        (Program [node (FunctionDefinition "f" (node (ELambda (node (PId "%0")) (node (EMatch (node (ETuple [node (EId "%0")])) [(node (PTuple [node (PTuple [node (PId "a"), node (PLiteral (LFloat 0.3))])]), node (EId "a"))])))))])

patternTypeAnnotatedExpansionTest :: Spec
patternTypeAnnotatedExpansionTest = do
    testAST "f (x: Integer) = x" "should be correctly expanded into a type annotated pattern"
        (Program [node (FunctionDefinition "f" (node (ELambda (node (PId "%0")) (node (EMatch (node (ETuple [node (EId "%0")])) [(node (PTuple [node (PTypeAnnotated (node (PId "x")) (node (TId "Integer")))]), node (EId "x"))])))))])

patternConstructorExpansionTest :: Spec
patternConstructorExpansionTest = do
    testAST "f (Cons x xs) = x" "should be correctly expanded into a constructor pattern"
        (Program [node (FunctionDefinition "f" (node (ELambda (node (PId "%0")) (node (EMatch (node (ETuple [node (EId "%0")])) [(node (PTuple [node (PConstructor "Cons" [node (PId "x"), node (PId "xs")])]), node (EId "x"))])))))])

patternOperatorsExpansionTest :: Spec
patternOperatorsExpansionTest = do
    testAST "f (x `Cons` xs) = x" "should be correctly expanded into a constructor pattern"
        (Program [node (FunctionDefinition "f" (node (ELambda (node (PId "%0")) (node (EMatch (node (ETuple [node (EId "%0")])) [(node (PTuple [node (PConstructor "Cons" [node (PId "x"), node (PId "xs")])]), node (EId "x"))])))))])

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