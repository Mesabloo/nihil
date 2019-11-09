-- iBlob, a REPL using the Blob programming language's interpreter.
-- Copyright (c) 2019 Mesabloo

-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.

-- You should have received a copy of the GNU General Public License
-- along with this program. If not, see <http://www.gnu.org/licenses/>.

{-# LANGUAGE OverloadedStrings, LambdaCase, TypeFamilies, BlockArguments #-}

-- | This module holds the command parser for the REPL.
module Blob.REPL.Commands where

import Text.Megaparsec
import Data.String.Utils (rstrip)
import Data.List (isInfixOf, intercalate)
import Data.Functor (($>))
import Control.Applicative (liftA2)
import Blob.REPL.Types
import qualified Data.Char as Ch
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L

-- | A list of all existing commands.
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
           ,   ":env"
           ,     ":!" ]

-- | The 'Help' command parser.
--
-- Either @:h@ or @:help@ or @:?@.
help :: Parser Command
help = C.space *> (try . hidden) (keyword "?" <|> keyword "help" <|> keyword "h") <* C.space $> Help <?> "߷"

-- | The 'Exit' command parser.
--
-- Either @:quit@ or @:q@.
exit :: Parser Command
exit = C.space *> (try . hidden) (keyword "quit" <|> keyword "q") <* C.space $> Exit <?> "߷"

-- | The 'Load' command parser.
--
-- Either @:load@ or @:l@.
load :: Parser Command
load = do
    C.space *> (try . hidden) (keyword "load" <|> keyword "l") <* C.space <?> "߷"

    observing (lookAhead eof) >>= \case
        Right _ -> fail "Missing argument \"[file]\""
        Left _  -> do
            file <- anySingle `someTill` eof
            pure . Load $ rstrip file

-- | When the entire input is some 'Code'.
code :: Parser Command
code = (eof *> fail "") <|> Code <$> anySingle `someTill` eof

-- | The 'ResetEnv' command parser.
--
-- Either @:r@ or @:reset@
reset :: Parser Command
reset = (C.space *> (try . hidden) (keyword "reset" <|> keyword "r") <* C.space <?> "߷")
     *> (ResetEnv <$> many (C.space *> (identifier <|> opSymbol) <* C.space))

-- | The 'GetType' command parser.
--
-- Either @:type@ or @:t@.
getType :: Parser Command
getType = do
    C.space *> (try . hidden) (keyword "type" <|> keyword "t") <* C.space <?> "߷"

    observing (lookAhead eof) >>= \case
        Right _ -> fail "Missing argument \"[expr]\""
        Left _  -> GetType <$> (anySingle `someTill` eof)

-- | The 'GetKind' command parser.
--
-- Either @:k@ or @:kind@.
getKind :: Parser Command
getKind = do
    C.space *> (try . hidden) (keyword "kind" <|> keyword "k") <* C.space <?> "߷"

    observing (lookAhead eof) >>= \case
        Right _ -> fail "Missing argument \"[type]\""
        Left _  -> GetKind <$> (anySingle `someTill` eof)

-- | The 'Time' command parser.
--
-- @:time@
time :: Parser Command
time = do
    C.space *> (try . hidden) (keyword "time") <* C.space <?> "߷"

    observing (lookAhead eof) >>= \case
        Right _ -> fail "Missing argument \"[expr]\""
        Left _  -> Time <$> (anySingle `someTill` eof)

-- | The 'Bench' command parser.
--
-- @:bench@
bench :: Parser Command
bench = do
    C.space *> (try . hidden) (keyword "bench") <* C.space <?> "߷"

    observing (lookAhead eof) >>= \case
        Right _ -> fail "Missing arguments \"[n] [expr]\""
        Left _  -> do
            n    <- L.decimal
            end' <- observing . lookAhead $ eof
            case end' of
                Right _ -> fail "Missing argument \"[expr]\""
                Left _  -> Bench n <$> (anySingle `someTill` eof)

-- | The 'Env' command parser.
--
-- @:env@
env :: Parser Command
env = C.space *> (try . hidden) (keyword "env") <* C.space $> Env <?> "߷"

-- | The 'Shell' command parser.
--
-- @:!@
shell :: Parser Command
shell = do
    C.space *> (try . hidden) (keyword "!") <* C.space <?> "߷"

    observing (lookAhead eof) >>= \case
        Right _ -> fail "Missing argument \"[command]\""
        Left _  -> Shell <$> (anySingle `someTill` eof)

-- | The global command parser.
command :: Parser Command
command = do { try (C.space *> C.string ":")
             ; cmd <- observing . try $ choice [help, exit, load, time, getType, getKind, reset, bench, env, shell] <* eof
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


-- | The levenshtein algorithm for suggesting typing error possibilities.
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

-- | A wrapper around the levenshtein algorithm to prettify the output.
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