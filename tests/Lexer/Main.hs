{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}

module Main where

import Test.Hspec
import Data.Foldable (sequenceA_)
import Blob.Language (runLexer)
import Blob.Language.Syntax.Tokens.Token
import Blob.Language.Syntax.Tokens.Lexeme
import Control.Lens.Prism
import Control.Lens
import Control.Applicative ((<|>))

main :: IO ()
main = hspec $ sequenceA_ tests

hasNoLexingError :: Either a b -> Bool
hasNoLexingError (Left  _) = False
hasNoLexingError (Right _) = True

getToken :: Token -> Maybe Lexeme
getToken = (^. to getLexeme)

shouldContainAll :: (Show a, Eq a) => [a] -> [a] -> Expectation
shouldContainAll [] [] = pure ()
shouldContainAll [] ys = expectationFailure ("[] was expected to contain " <> show ys <> ".")
shouldContainAll _  [] = pure ()
shouldContainAll xs (y:ys) =
    (((y `elem` xs) `shouldBe` True)
      <|> expectationFailure (show xs <> " was expected to contain " <> show y <> "."))
    *> shouldContainAll xs ys

shouldNotContainAnyOf :: (Show a, Eq a) => [a] -> [a] -> Expectation
shouldNotContainAnyOf [] [] = pure ()
shouldNotContainAnyOf [] _  = pure ()
shouldNotContainAnyOf _  [] = pure ()
shouldNotContainAnyOf xs (y:ys) =
    (((y `elem` xs) `shouldNotBe` True)
      <|> expectationFailure (show xs <> " was expected not to contain " <> show y <> "."))
    *> shouldNotContainAnyOf xs ys

--------------------------------------------------------------------

tests :: [Spec]
tests = [ test1, test2, test3, test4, test5, test6 ]

test1 :: Spec
test1 = describe "Test on empty stream" $
    let res = runLexer "" ""
    in do it "should not error out" $
            res `shouldSatisfy` hasNoLexingError
          let x = views _Right (getToken <$>) res
          it "returns nothing" $
            x `shouldBe` []

test2 :: Spec
test2 = describe "Test on blank stream" $
    let res = runLexer "   \t\n\t   \n" ""
    in do it "should not error out" $
            res `shouldSatisfy` hasNoLexingError
          let x = views _Right (getToken <$>) res
          it "returns `Nothing`s" $
            x `shouldBe` [Nothing, Nothing]

test3 :: Spec
test3 = describe "Test on atoms" $
    let res = runLexer "'e' \"xyz\" 032 0.3" ""
    in do it "should not error out" $
            res `shouldSatisfy` hasNoLexingError
          let x = views _Right (getToken <$>) res
          it "returns 4 tokens" $
            x `shouldBe` [Just (LChar 'e'), Just (LString "xyz"), Just (LInteger 32), Just (LFloat 0.3)]

test4 :: Spec
test4 = describe "Test on malformed atoms" $
    let res1 = runLexer "'ee'" ""
        res2 = runLexer "\"xxx" ""
        res3 = runLexer "0." ""
    in do it "should not lex a character" $
            res1 `shouldNotSatisfy` hasNoLexingError
          it "should not lex a string" $
            res2 `shouldNotSatisfy` hasNoLexingError
          it "should not lex a floating point number" $ do
            res3 `shouldSatisfy` hasNoLexingError
            let x = views _Right (getToken <$>) res3
            x `shouldNotContainAnyOf` [Just (LFloat 0.0)]

test5 :: Spec
test5 = parallel $ describe "Test on keywords and identifiers" $
    let res = runLexer "match i with wy datax Nothing" ""
    in do it "should not error out" $
            res `shouldSatisfy` hasNoLexingError
          let x = views _Right (getToken <$>) res
          it "contains 2 keywords (match, with)" $
            x `shouldContainAll` [Just (LKeyword "match"), Just (LKeyword "with")]
          it "contains 3 identifiers" $
            x `shouldContainAll` [Just (LLowIdentifier "i"), Just (LLowIdentifier "wy")
                                 , Just (LLowIdentifier "datax"), Just (LUpIdentifier "Nothing")]

test6 :: Spec
test6 = describe "Test on wildcards" $
    let res = runLexer "__ _" ""
    in do it "should not error out" $
            res `shouldSatisfy` hasNoLexingError
          let x = views _Right (getToken <$>) res
          it "contains 2 wildcards" $
            x `shouldContainAll` [Just LWildcard, Just LWildcard]