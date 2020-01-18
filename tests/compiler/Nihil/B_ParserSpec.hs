{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Nihil.B_ParserSpec (spec) where

import Test.Hspec
import Nihil.Syntax (runParser, runLexer)
import Nihil.Syntax.Concrete.Core
import Nihil.Utils.Source (locate, SourcePos(NoSource), Located)
import qualified Data.Text as Text (Text)
import qualified Data.Map as Map (fromList)

spec :: Spec
spec = parallel do
    describe "Test on blank stream" blankInputStreamTest

    describe "Tests on types" do
        describe "Test on unit type"          typeUnitDeclarationTest
        describe "Test on type identifier"    typeIdentifierDeclarationTest
        describe "Test on type variable"      typeVariableDeclarationTest
        describe "Test on type tuple"         typeTupleDeclarationTest
        describe "Test on type application"   typeApplicationDeclarationTest
        describe "Test on type operator"      typeOperatorDeclarationTest
        describe "Test on parenthesized type" typeParensDeclarationTest
    describe "Tests on patterns" do
        describe "Test on unit pattern"           patternUnitMatchTest
        describe "Test on literal pattern"        patternLiteralMatchTest
        describe "Test on identifier pattern"     patternIdentifierMatchTest
        describe "Test on wildcard pattern"       patternWildcardMatchTest
        describe "Test on tuple pattern"          patternTupleMatchTest
        describe "Test on parenthesized pattern"  patternParensMatchTest
        describe "Test on type annotated pattern" patternTypeAnnotatedMatchTest
        describe "Test on constructor pattern"    patternConstructorMatchTest
        describe "Test on pattern operator"       patternOperatorMatchTest
    describe "Tests on expressions" do
        describe "Test on literal expression"         expressionLiteralValueTest
        describe "Test on identifier expression"      expressionIdentifierValueTest
        describe "Test on typehole expression"        expressionTypeholeValueTest
        describe "Test on unit expression"            expressionUnitValueTest
        describe "Test on tuple expression"           expressionTupleValueTest
        describe "Test on expression operator"        expressionOperatorValueTest
        xdescribe "Test on type annotated expression"  expressionTypeAnnotatedValueTest
        describe "Test on expression application"     expressionApplicationValueTest
        describe "Test on parenthesized expression"   expressionParensValueTest
        xdescribe "Test on let expression"             expressionLetInValueTest
        xdescribe "Test on where expression"           expressionWhereValueTest
        describe "Test on lambda expression"           expressionLambdaValueTest
        describe "Test on match expression"            expressionMatchWithValueTest
    describe "Test on top-level statements" do
        describe "Test on function declaration"        toplevelFunctionDeclarationTest
        describe "Test on function definition"         toplevelFunctionDefinitionTest
        describe "Test on operator fixity declaration" toplevelOperatorFixityDeclarationTest
        describe "Test on custom types declaration"    toplevelCustomTypesDeclarationTest

blankInputStreamTest :: Spec
blankInputStreamTest = do
    let (Right lex) = runLexer "" "test"
    let (Left ____) = runParser lex "test"
    pure ()

typeUnitDeclarationTest :: Spec
typeUnitDeclarationTest = do
    testAST "type F = ()" "type tuple"
        (Program [node (TypeDefinition "F" [] (node (TypeAlias [node (TTuple [])])))])

typeIdentifierDeclarationTest :: Spec
typeIdentifierDeclarationTest = do
    testAST "type G = Char" "type identifier"
        (Program [node (TypeDefinition "G" [] (node (TypeAlias [node (TId "Char")])))])

typeVariableDeclarationTest :: Spec
typeVariableDeclarationTest = do
    testAST "absurd: a" "type variable"
        (Program [node (FunDeclaration "absurd" [node (TVar "a")])])

typeTupleDeclarationTest :: Spec
typeTupleDeclarationTest = do
    testAST "f: (a, b)" "type tuple"
        (Program [node (FunDeclaration "f" [node (TTuple [[node (TVar "a")], [node (TVar "b")]])])])

typeApplicationDeclarationTest :: Spec
typeApplicationDeclarationTest = do
    testAST "g: Maybe a" "type application"
        (Program [node (FunDeclaration "g" [node (TApplication [node (TId "Maybe"), node (TVar "a")])])])

typeOperatorDeclarationTest :: Spec
typeOperatorDeclarationTest = do
    testAST "type X w x = w ~~ x" "type operator"
        (Program [node (TypeDefinition "X" ["w", "x"] (node (TypeAlias [node (TVar "w"), node (TOperator "~~"), node (TVar "x")])))])

typeParensDeclarationTest :: Spec
typeParensDeclarationTest = do
    testAST "type X a b = Maybe (a)" "parenthesized type"
        (Program [node (TypeDefinition "X" ["a", "b"] (node (TypeAlias [node (TApplication [node (TId "Maybe"), node (TParens [node (TVar "a")])])])))])



patternUnitMatchTest :: Spec
patternUnitMatchTest = do
    testAST "f = \\() -> 0" "unit pattern"
        (Program [node (FunDefinition "f" [] (node [node (ALambda [node (PTuple [])] (node [node (ALiteral (LInteger 0))]))]))])

patternLiteralMatchTest :: Spec
patternLiteralMatchTest = do
    testAST "g = \\0 -> 0" "literal pattern"
        (Program [node (FunDefinition "g" [] (node [node (ALambda [node (PLiteral (LInteger 0))] (node [node (ALiteral (LInteger 0))]))]))])

patternIdentifierMatchTest :: Spec
patternIdentifierMatchTest = do
    testAST "f = \\id -> 0" "identifier pattern"
        (Program [node (FunDefinition "f" [] (node [node (ALambda [node (PId "id")] (node [node (ALiteral (LInteger 0))]))]))])

patternWildcardMatchTest :: Spec
patternWildcardMatchTest = do
    testAST "f = \\_ -> 0" "wildcard pattern"
        (Program [node (FunDefinition "f" [] (node [node (ALambda [node PWildcard] (node [node (ALiteral (LInteger 0))]))]))])

patternTupleMatchTest :: Spec
patternTupleMatchTest = do
    testAST "f = \\(a, b) -> 0" "tuple pattern"
        (Program [node (FunDefinition "f" [] (node [node (ALambda [node (PTuple [[node (PId "a")], [node (PId "b")]])] (node [node (ALiteral (LInteger 0))]))]))])

patternParensMatchTest :: Spec
patternParensMatchTest = do
    testAST "f (a) = 0" "parenthesized pattern"
        (Program [node (FunDefinition "f" [node (PParens [node (PId "a")])] (node [node (ALiteral (LInteger 0))]))])

patternTypeAnnotatedMatchTest :: Spec
patternTypeAnnotatedMatchTest = do
    testAST "g = \\(x: Char) -> 0" "type annotated pattern"
        (Program [node (FunDefinition "g" [] (node [node (ALambda [node (PParens [node (PTypeAnnotated [node (PId "x")] [node (TId "Char")])])] (node [node (ALiteral (LInteger 0))]))]))])

patternConstructorMatchTest :: Spec
patternConstructorMatchTest = do
    testAST "h (Cons x xs) = 0" "constructor pattern"
        (Program [node (FunDefinition "h" [node (PParens [node (PConstructor "Cons" [node (PId "x"), node (PId "xs")])])] (node [node (ALiteral (LInteger 0))]))])

patternOperatorMatchTest :: Spec
patternOperatorMatchTest = do
    testAST "x (a `Cons` b) = 0" "pattern operator"
        (Program [node (FunDefinition "x" [node (PParens [node (PId "a"), node (POperator "Cons"), node (PId "b")])] (node [node (ALiteral (LInteger 0))]))])



expressionLiteralValueTest :: Spec
expressionLiteralValueTest = do
    testAST "w = 0.36" "literal expression"
        (Program [node (FunDefinition "w" [] (node [node (ALiteral (LDouble 0.36))]))])

expressionIdentifierValueTest :: Spec
expressionIdentifierValueTest = do
    testAST "x = y" "identifier expression"
        (Program [node (FunDefinition "x" [] (node [node (AId "y")]))])

expressionTypeholeValueTest :: Spec
expressionTypeholeValueTest = do
    testAST "x = _" "typehole expression"
        (Program [node (FunDefinition "x" [] (node [node ATypeHole]))])

expressionUnitValueTest :: Spec
expressionUnitValueTest = do
    testAST "unit = ()" "unit expression"
        (Program [node (FunDefinition "unit" [] (node [node (ATuple [])]))])

expressionTupleValueTest :: Spec
expressionTupleValueTest = do
    testAST "k = (0, 3, 'c')" "tuple expression"
        (Program [node (FunDefinition "k" [] (node [node (ATuple [node [node (ALiteral (LInteger 0))], node [node (ALiteral (LInteger 3))], node [node (ALiteral (LCharacter 'c'))]])]))])

expressionOperatorValueTest :: Spec
expressionOperatorValueTest = do
    testAST "k = f >=> g" "expression operator"
        (Program [node (FunDefinition "k" [] (node [node (AId "f"), node (AOperator ">=>"), node (AId "g")]))])

expressionTypeAnnotatedValueTest :: Spec
expressionTypeAnnotatedValueTest = do
    testAST "s = 0 : Integer" "type annotated expression"
        (Program [node (FunDefinition "s" [] (node [node (ATypeAnnotated (node [node (AId "s")]) [node (TId "Integer")])]))])

expressionApplicationValueTest :: Spec
expressionApplicationValueTest = do
    testAST "m = f x" "expression application"
        (Program [node (FunDefinition "m" [] (node [node (AApplication [node (AId "f"), node (AId "x")])]))])

expressionParensValueTest :: Spec
expressionParensValueTest = do
    testAST "m = (0 + 1)" "parenthesized expression"
        (Program [node (FunDefinition "m" [] (node [node (AParens (node [node (ALiteral (LInteger 0)), node (AOperator "+"), node (ALiteral (LInteger 1))]))]))])

expressionLetInValueTest :: Spec
expressionLetInValueTest = do
    testAST "g = let f = 0\n        h = f in h" "let expression"
        (Program [node (FunDefinition "g" [] (node [node (ALet [node (FunDefinition "f" [] (node [node (ALiteral (LInteger 0))])), node (FunDefinition "h" [] (node [node (AId "f")]))] (node [node (AId "h")]))]))])

expressionWhereValueTest :: Spec
expressionWhereValueTest = do
    testAST "w = f where g = 0\n            f = g" "where expression"
        (Program [node (FunDefinition "w" [] (node [node (AWhere (node [node (AId "f")]) [node (FunDefinition "g" [] (node [node (ALiteral (LInteger 0))])), node (FunDefinition "f" [] (node [node (AId "g")]))])]))])

expressionLambdaValueTest :: Spec
expressionLambdaValueTest = do
    testAST "f = \\_ -> \"test\"" "lambda expression"
        (Program [node (FunDefinition "f" [] (node [node (ALambda [node PWildcard] (node [node (ALiteral (LString "test"))]))]))])

expressionMatchWithValueTest :: Spec
expressionMatchWithValueTest = do
    testAST "f = match 0 with\n    0 -> 0\n    _ -> 1" "match expression"
        (Program [node (FunDefinition "f" [] (node [node (AMatch (node [node (ALiteral (LInteger 0))]) [([node (PLiteral (LInteger 0))], node [node (ALiteral (LInteger 0))]), ([node PWildcard], node [node (ALiteral (LInteger 1))])])]))])



toplevelFunctionDeclarationTest :: Spec
toplevelFunctionDeclarationTest = do
    testAST "f: Integer -> Integer" "function declaration"
        (Program [node (FunDeclaration "f" [node (TId "Integer"), node (TOperator "->"), node (TId "Integer")])])

toplevelFunctionDefinitionTest :: Spec
toplevelFunctionDefinitionTest = do
    testAST "f x = x" "function definition"
        (Program [node (FunDefinition "f" [node (PId "x")] (node [node (AId "x")]))])

toplevelOperatorFixityDeclarationTest :: Spec
toplevelOperatorFixityDeclarationTest = do
    testAST "infixl 4 -+*" "operator fixity declaration"
        (Program [node (OperatorFixity "-+*" (node (Infix L 4)))])

toplevelCustomTypesDeclarationTest :: Spec
toplevelCustomTypesDeclarationTest = do
    describe "Test on sumtype" do
        testAST "data Maybe a = Nothing | Just a" "sum-type"
            (Program [node (TypeDefinition "Maybe" ["a"] (node (SumType (Map.fromList [("Nothing", []), ("Just", [node (TVar "a")])]))))])
    describe "Test on type alias" do
        testAST "type X = Y" "type alias"
            (Program [node (TypeDefinition "X" [] (node (TypeAlias [node (TId "Y")])))])
    describe "Test on GADT" do
        testAST "data X where Y: X" "GADT"
            (Program [node (TypeDefinition "X" [] (node (GADT (Map.fromList [("Y", [node (TId "X")])]))))])

---------------------------------------------------------------------------------------------

node :: a -> Located a
node = (`locate` NoSource)

testAST :: Text.Text -> String -> Program -> Spec
testAST code name expected = do
    let (Right lex) = runLexer code "test"
    let (Right ast) = runParser lex "test"
    it ("should return a well-formed " <> name) do
        ast `shouldBe` expected