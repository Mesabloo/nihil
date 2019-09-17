{-# LANGUAGE OverloadedStrings, LambdaCase, TypeFamilies, BlockArguments #-}

module Blob.REPL.Commands where

import qualified Data.Map as Map
import Text.Megaparsec 
import Data.String.Utils (rstrip)
import Control.Monad.Reader (local, asks, ask)
import Control.Monad.Except (throwError)
import Data.List (isInfixOf, intercalate)
import Data.Maybe (fromJust, isJust)
import Text.PrettyPrint.Leijen (text, Doc, dot, linebreak)
import System.Exit (exitSuccess)
import System.Console.ANSI (setSGR, SGR(..), ConsoleLayer(..), ColorIntensity(..), Color(..), ConsoleIntensity(..))
import Data.Functor (($>))
import Data.Char (isUpper)
import Control.Monad (join, zipWithM)
import Control.Applicative (empty, liftA2)
import Data.List.Extra
import Debug.Trace
import Blob.REPL.Types
import qualified Data.Text as Text
import qualified Data.Char as Ch
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L

commands :: [String]
commands = [  ":help", ":h", ":?"
           ,  ":quit", ":q"
           ,  ":load", ":l" 
           ,  ":type", ":t"
           ,  ":kind", ":k"
           ,  ":eval", ":ev"
           , ":reset", ":r"
           ,  ":time"
           , ":bench"
           ,   ":env" ]

help :: Parser Command
help = C.space *> (try . hidden) (keyword "?" <|> keyword "help" <|> keyword "h") <* C.space $> Help <?> "߷"

exit :: Parser Command
exit = C.space *> (try . hidden) (keyword "quit" <|> keyword "q") <* C.space $> Exit <?> "߷"

load :: Parser Command
load = do
    C.space *> (try . hidden) (keyword "load" <|> keyword "l") <* C.space <?> "߷"

    end <- observing . lookAhead $ eof
    case end of
        Right _ -> fail "Missing argument \"[file]\""
        Left _  -> do
            file <- anySingle `someTill` eof
            pure . Load $ rstrip file

code :: Parser Command
code = (eof *> fail "") <|> Code <$> anySingle `someTill` eof

reset :: Parser Command
reset = (C.space *> (try . hidden) (keyword "reset" <|> keyword "r") <* C.space <?> "߷") *> (ResetEnv <$> many (C.space *> (identifier <|> opSymbol) <* C.space))

getType :: Parser Command
getType = do
    C.space *> (try . hidden) (keyword "type" <|> keyword "t") <* C.space <?> "߷"

    end <- observing . lookAhead $  eof
    case end of
        Right _ -> fail "Missing argument \"[expr]\""
        Left _  -> GetType <$> (anySingle `someTill` eof)

getKind :: Parser Command
getKind = do
    C.space *> (try . hidden) (keyword "kind" <|> keyword "k") <* C.space <?> "߷"

    end <- observing . lookAhead $ eof
    case end of
        Right _ -> fail "Missing argument \"[type]\""
        Left _  -> GetKind <$> (anySingle `someTill` eof)

time :: Parser Command
time = do
    C.space *> (try . hidden) (keyword "time") <* C.space <?> "߷"

    end <- observing . lookAhead $ eof
    case end of
        Right _ -> fail "Missing argument \"[expr]\""
        Left _  -> Time <$> (anySingle `someTill` eof)

bench :: Parser Command
bench = do
    C.space *> (try . hidden) (keyword "bench") <* C.space <?> "߷"

    end <- observing . lookAhead $ eof
    case end of
        Right _ -> fail "Missing arguments \"[n] [expr]\""
        Left _  -> do
            n    <- L.decimal
            end' <- observing . lookAhead $ eof
            case end' of
                Right _ -> fail "Missing argument \"[expr]\""
                Left _  -> Bench n <$> (anySingle `someTill` eof)

env :: Parser Command
env = C.space *> (try . hidden) (keyword "env") <* C.space $> Env <?> "߷"

command :: Parser Command
command = do { try (C.space *> C.string ":")
             ; cmd <- observing . try $ choice [help, exit, load, time, getType, getKind, reset, bench, env] <* eof
             ; case cmd of
                Left err ->
                    if "߷" `isInfixOf` parseErrorTextPretty err
                    then do
                        msg <- makeCommandError
                        fail msg
                    else fail $ parseErrorTextPretty err
                Right x  -> pure x
             } <|> C.space *> code

makeCommandError :: Parser String
makeCommandError = do
    cmd <- C.space *> manyTill anySingle (C.space1 <|> eof)
    pure $ "Unknown command `" <> cmd <> "`\n" <> maybeYouWanted (':':cmd) commands



levenshtein :: String -> String -> Int
levenshtein s1 s2
    | length s1 > length s2 = levenshtein s2 s1
    | length s1 < length s2 = let d = length s2 - length s1
                              in  d + levenshtein s1 (take (length s2 - d) s2)
levenshtein "" ""           = 0
levenshtein s1 s2
    | last s1 == last s2    = levenshtein (init s1) (init s2)
    | otherwise             = minimum [ 1 + levenshtein (init s1) s2
                                      , 1 + levenshtein s1 (init s2)
                                      , 1 + levenshtein (init s1) (init s2) ]

maybeYouWanted :: String -> [String] -> String
maybeYouWanted source choices = 
    let s = intercalate ", "
            . takeMax 3
            . filter (/= "")
            . map (\item ->
                if levenshtein source item > 3
                then ""
                else intercalate item ["\"", "\""])
            $ choices
                                        
    in
        if s /= ""
        then "Perhaps you meant " <> s <> "?"
        else ""
  where takeMax :: Int -> [a] -> [a]
        takeMax n xs | n > length xs = xs
                     | otherwise     = take n xs

-------------------------------------------------------------------------------------------------------------

keyword :: String -> Parser String
keyword s = C.string s <* notFollowedBy (satisfy $ liftA2 (&&) Ch.isPrint (not . Ch.isSpace))

identifier :: Parser String
identifier = (:) <$> C.lowerChar <*> many (C.alphaNumChar <|> satisfy (liftA2 (||) (== '_') (== '\'')))

opSymbol :: Parser String
opSymbol = parens p <|> p
  where p = some $ C.symbolChar <|> oneOf ("!#$%&.<=>?^~|@*/-:" :: String)
        parens p = C.char '(' *> C.space *> p <* C.space <* C.char ')'