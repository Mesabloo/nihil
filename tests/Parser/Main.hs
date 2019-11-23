{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Hspec
import Control.Applicative ((<|>))
import Data.Foldable (sequenceA_, forM_)
import Blob.Language (runParser, runLexer, runParser')
import Blob.Language.Syntax.Rules.Lexing.Keyword (kwords)
import Blob.Language.Syntax.Tokens.Lexeme (Lexeme(..))
import Blob.Language.Syntax.Tokens.Token (getLexeme)
import Blob.Language.Syntax.Rules.Parsing.Keyword (keyword)
import Text.Megaparsec.Pos (mkPos)
import Control.Lens
import qualified Data.Text as Text
import Data.Maybe (fromJust)

main :: IO ()
main = hspec (sequenceA_ tests)

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

hasParseError :: Either a b -> Bool
hasParseError (Left _) = True
hasParseError (Right _) = False

---------------------------------------------------------------------------------------------

tests :: [Spec]
tests = [ test1, test2, test3 ]

test1 :: Spec
test1 = describe "Test on empty stream" $
    let res = runParser [] ""
    in do it "should not error out" $
            res `shouldNotSatisfy` hasParseError
          let x = res ^. _Right
          it "returns nothing" $
            x `shouldBe` []

test2 :: Spec
test2 = describe "Test on keywords" $
    let toParse = view _Right . flip runLexer "" <$> kwords
        getRes n = runParser' (keyword . Text.unpack $ kwords !! n) (toParse !! n) ""

        isKeyword (LKeyword _) = True
        isKeyword _ = False
    in forM_ [0..length toParse - 1] $ \n ->
          describe (show $ kwords !! n) $ do
              let res = getRes n
              it "should not error out" $
                res `shouldNotSatisfy` hasParseError
              let (Just x) = res ^? _Right . to getLexeme . to fromJust
              it "is parsed as a keyword" $
                x `shouldSatisfy` isKeyword

test3 :: Spec
test3 = pure ()