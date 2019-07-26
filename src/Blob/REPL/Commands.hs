{-# LANGUAGE OverloadedStrings, LambdaCase #-}

module Blob.REPL.Commands where

import Blob.Language.Parsing.Types (Parser, Expr(..), Literal(..), Pattern(..))
import Blob.Language.Parsing.Lexer (string', space', space1', symbol, integer, keyword, lexemeN, identifier, opSymbol)
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
import Control.Applicative (empty)
import Data.List.Extra
import Debug.Trace
import Blob.REPL.Types


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
help = space' *> (try . hidden . lexemeN) (keyword "?" <|> keyword "help" <|> keyword "h") $> Help <?> "߷"

exit :: Parser Command
exit = space' *> (try . hidden . lexemeN) (keyword "quit" <|> keyword "q") $> Exit <?> "߷"

load :: Parser Command
load = do
    space' *> (try . hidden . lexemeN) (keyword "load" <|> keyword "l") <?> "߷"

    end <- observing . lookAhead $ eof
    case end of
        Right _ -> fail "Missing argument “[file]”"
        Left _  -> do
            file <- anySingle `someTill` eof
            pure . Load $ rstrip file

code :: Parser Command
code = (eof *> fail "") <|> Code <$> anySingle `someTill` eof

reset :: Parser Command
reset = (space' *> (try . hidden . lexemeN) (keyword "reset" <|> keyword "r") <?> "߷") >> (ResetEnv <$> many (identifier <|> opSymbol))

getType :: Parser Command
getType = do
    space' *> (try . hidden . lexemeN) (keyword "type" <|> keyword "t") <?> "߷"

    end <- observing . lookAhead $  eof
    case end of
        Right _ -> fail "Missing argument “[expr]”"
        Left _  -> GetType <$> (anySingle `someTill` eof)

getKind :: Parser Command
getKind = do
    space' *> (try . hidden . lexemeN) (keyword "kind" <|> keyword "k") <?> "߷"

    end <- observing . lookAhead $ eof
    case end of
        Right _ -> fail "Missing argument “[type]”"
        Left _  -> GetKind <$> (anySingle `someTill` eof)

time :: Parser Command
time = do
    space' *> (try . hidden . lexemeN) (keyword "time") <?> "߷"

    end <- observing . lookAhead $ eof
    case end of
        Right _ -> fail "Missing argument “[expr]”"
        Left _  -> Time <$> (anySingle `someTill` eof)

bench :: Parser Command
bench = do
    space' *> (try . hidden . lexemeN) (keyword "bench") <?> "߷"

    end <- observing . lookAhead $ eof
    case end of
        Right _ -> fail "Missing arguments “[n] [expr]”"
        Left _  -> do
            n    <- integer
            end' <- observing . lookAhead $ eof
            case end' of
                Right _ -> fail "Missing argument “[expr]”"
                Left _  -> Bench n <$> (anySingle `someTill` eof)

env :: Parser Command
env = space' *> (try . hidden . lexemeN) (keyword "env") $> Env <?> "߷"

command :: Parser Command
command = do { space' *> symbol ":"
             ; cmd <- observing . try $ choice [help, exit, load, time, getType, getKind, reset, bench, env] <* eof
             ; case cmd of
                Left err ->
                    if "߷" `isInfixOf` parseErrorTextPretty err
                    then do
                        msg <- makeCommandError
                        fail msg
                    else fail $ parseErrorTextPretty err
                Right x  -> pure x
             } <|> code

makeCommandError :: Parser String
makeCommandError = do
    cmd <- space' *> manyTill anySingle (space1' <|> eof)
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
                else intercalate item ["“", "”"])
            $ choices
                                        
    in
        if s /= ""
        then "Perhaps you meant " <> s <> "?"
        else ""
  where takeMax :: Int -> [a] -> [a]
        takeMax n xs | n > length xs = xs
                     | otherwise     = take n xs