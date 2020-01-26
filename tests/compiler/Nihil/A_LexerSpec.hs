{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Nihil.A_LexerSpec (spec) where

import Test.Hspec
import Nihil.Syntax (runLexer)
import Nihil.Syntax.Concrete.Lexeme
import Nihil.Utils.Source (locate, SourcePos(NoSource))

spec :: Spec
spec = parallel do
    -- blank output tests
    describe "Test on no input"             noInputStreamTest
    describe "Test on blank input"          invisibleInputStreamTest
    describe "Test on comment stream"       commentInputStreamTest

    -- other tests
    describe "Test on characters"           characterInputStreamTest
    describe "Test on strings"              stringInputStreamTest
    describe "Test on symbols"              symbolInputStreamTest
    describe "Test on function identifiers" funIdentifierInputStreamTest
    describe "Test on type identifiers"     typeIdentifierInputStreamTest
    describe "Test on numbers"              numberInputStreamTest
    describe "Test on floats"               floatInputStreamTest

    describe "Test on keywords"             keywordInputStreamTest

noInputStreamTest :: Spec
noInputStreamTest = do
    let (Right lex) = runLexer "" "test"
    it "should return no lexeme" do
        lex `shouldBe` []

invisibleInputStreamTest :: Spec
invisibleInputStreamTest = do
    let (Right lex) = runLexer "\n\t\n\n\n    \t\t\n\n    " "test"
    it "should return no lexeme" do
        lex `shouldBe` []

commentInputStreamTest :: Spec
commentInputStreamTest = do
    let (Right lex) = runLexer "{- multiline\ncomment -} -- inline comment\n\t{- no -- inline comment -}" "test"
    it "should return no lexeme" do
        lex `shouldBe` []



characterInputStreamTest :: Spec
characterInputStreamTest = do
    let (Right lex) = runLexer "'a' '\\n' '\\'' 'é'" "test"
    it "should return well-formed characters" do
        lex `shouldBe` [tk (LChar 'a'), tk (LChar '\n'), tk (LChar '\''), tk (LChar 'é')]

stringInputStreamTest :: Spec
stringInputStreamTest = do
    let (Right lex) = runLexer "\" \" \"\\nHello, world!\'\" \"\" \"\nHello\"" "test"
    it "should return well-formed strings" do
        lex `shouldBe` [tk (LString " "), tk (LString "\nHello, world!'"), tk (LString ""), tk (LString "\nHello")]

symbolInputStreamTest :: Spec
symbolInputStreamTest = do
    let (Right lex) = runLexer "{()}++ @^" "test"
    it "should return well-formed symbols" do
        lex `shouldBe` [tk (LSymbol "{"), tk (LSymbol "("), tk (LSymbol ")"), tk (LSymbol "}"), tk (LSymbol "++"), tk (LSymbol "@^")]

funIdentifierInputStreamTest :: Spec
funIdentifierInputStreamTest = do
    let (Right lex) = runLexer "f g wlhwkjhw δνμσ matchf" "test"
    it "should return well-formed function identifiers" do
        lex `shouldBe` [tk (LLowerIdentifier "f"), tk (LLowerIdentifier "g"), tk (LLowerIdentifier "wlhwkjhw"), tk (LLowerIdentifier "δνμσ"), tk (LLowerIdentifier "matchf")]

typeIdentifierInputStreamTest :: Spec
typeIdentifierInputStreamTest = do
    let (Right lex) = runLexer "A Test Δtest Βλob" "test"
    it "should return well-formed type identifiers" do
        lex `shouldBe` [tk (LUpperIdentifier "A"), tk (LUpperIdentifier "Test"), tk (LUpperIdentifier "Δtest"), tk (LUpperIdentifier "Βλob")]

numberInputStreamTest :: Spec
numberInputStreamTest = do
    let (Right lex) = runLexer "0x0 0b00001111 7 0X2 0o7 0B1 256 0O7" "test"
    it "should return well-formed numbers" do
        lex `shouldBe` [tk (LInteger 0), tk (LInteger 15), tk (LInteger 7), tk (LInteger 2), tk (LInteger 7), tk (LInteger 1), tk (LInteger 256), tk (LInteger 7)]

floatInputStreamTest :: Spec
floatInputStreamTest = do
    let (Right lex) = runLexer "0.23 7.3e1 9.6e-1" "test"
    it "should return well-formed floating point numbers" do
        lex `shouldBe` [tk (LFloat 0.23), tk (LFloat 7.3e1), tk (LFloat 9.6e-1)]



keywordInputStreamTest :: Spec
keywordInputStreamTest = do
    let (Right lex) = runLexer "match data in where" "test"
    it "should return well-formed keywords" do
        lex `shouldBe` [tk (LKeyword "match"), tk (LKeyword "data"), tk (LKeyword "in"), tk (LKeyword "where")]

-----------------------------------------------------------------------------------------------

tk :: Lexeme -> Token
tk x = Token (Just (x `locate` NoSource))
{-# INLINE tk #-}