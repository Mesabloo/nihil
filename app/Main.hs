{-# LANGUAGE TypeApplications #-}

module Main where

import Options.Applicative
import Options.Flags

import Text.Diagnose

import System.IO

import Numeric (showIntAtBase)
import Data.Char (intToDigit)

main :: IO ()
main = do
  flags <- customExecParser pref opts


  let p1 = Position (1, 17) (1, 24) "./hello.nhl"
      p2 = Position (3, 9) (3, 12) "./hello.nhl"
      p3 = Position (10, 11) (10, 12) "./hello.nhl"
      p4 = Position (3, 1) (3, 2) "./hello.nhl"

      p5 = Position (5, 1) (5, 5) "./hello.nhl"
      p6 = Position (6, 1) (6, 5) "./hello.nhl"
      p7 = Position (7, 1) (7, 5) "./hello.nhl"
      p8 = Position (8, 1) (8, 5) "./hello.nhl"
  printDiagnostic stderr $
    diagnostic
    <++> reportError "Mismatched types. Expected `Integer`, found `String`."
                       [(p1, This "Found to be a `String`")
                       ,(p1, Where "Some important information")
                       ,(p1, Maybe "The function `read` might be relevant here")]
                       [hint "This most likely happen because you tried to give a type `a` to something that expected a type `b`. This is rejected by the type system because there is no possible implicit coercion between any types. Learn more about the error at <https://github.com/mesabloo/nihil/doc/hints/mismatched-types.md>."]
    <++> reportWarning "Implicit conversion from type `Char` to type `Integer`."
                       [(p2, This "Found to be a `Char`")
                       ,(p2, Maybe "This conversion can be made explicit with `ord`")]
                       [hint "Implicit conversions are rare, but do happen. You can learn more about them at <https://github.com/mesabloo/nihil/doc/hints/implicit-cast.md>."]
    <++> reportWarning "Implicit conversion from type `Integer` to type `Char`."
                       [(p4, Where "g :: Integer")
                       ,(p3, This "Should have been a `Char`")]
                       [hint "Same hint as above, I'm just lazy."]
    <++> reportError "Function `test` was expected to have 2 arguments, but only got 1."
                       [(p5, Empty)
                       ,(p6, Empty)
                       ,(p7, Empty)
                       ,(p8, This "Definition has 1 argument, whereas all others have 2 arguments. What's funny is that when the text becomes too long, it automatically correctly goes to a new line.")]
                       []
    <~< ("./hello.nhl", ["f = 1 + 2 + 3 + \"hello\"", "", "g = 1 - 'c'", "", "test 1 2 = 3", "test 3 4 = 5", "test 4 5 = 6", "test _   = 0", "", "h = 'c' + g"])



 where opts = info (pFlags <**> helper) fullDesc
       pref = prefs showHelpOnError
