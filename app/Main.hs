module Main where

import Options.Applicative
import Options.Flags

import Numeric (showIntAtBase)
import Data.Char (intToDigit)

main :: IO ()
main = do
  flags <- customExecParser pref opts

  putStrLn (showIntAtBase 2 intToDigit flags "")
  print (isFlagOn flags flag_Wall)
 where opts = info (pFlags <**> helper) fullDesc
       pref = prefs showHelpOnError
