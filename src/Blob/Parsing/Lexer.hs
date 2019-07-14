{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Blob.Parsing.Lexer where

import Text.Megaparsec (hidden, some, many, skipMany, skipSome, oneOf, try, (<?>), (<|>), between, manyTill, notFollowedBy, eof, empty)
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Pos (Pos, unPos)
import Blob.Parsing.Types (Parser)
import Data.Text (Text, pack, unpack)
import Control.Monad.State (get, lift, modify, gets, void)
import Data.Functor (($>))

eof :: Parser ()
eof = Text.Megaparsec.eof

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

lexemeN :: Parser a -> Parser a
lexemeN = L.lexeme scN

space' :: Parser ()
space' = skipMany C.spaceChar

sc :: Parser ()
sc = L.space space1' lineCmnt blockCmnt

scN :: Parser ()
scN = L.space (skipSome $ (C.eol $> '_') <|> oneOf (" \t" :: String)) lineCmnt blockCmnt

lineCmnt :: Parser ()
lineCmnt  = hidden $ L.skipLineComment (pack "--")

blockCmnt :: Parser ()
blockCmnt = lexeme . hidden $ L.skipBlockComment (pack "{-") (pack "-}")

space1' :: Parser ()
space1' = void . skipSome $ oneOf (" \t" :: String)

identifier :: Parser String
identifier = (lexemeN . try $ p >>= check) <?> "identifier"
  where
    p = (:)
            <$> C.lowerChar
            <*> many (C.alphaNumChar <|> C.digitChar <|> oneOf ("'_" :: String))
    check x = if x `elem` kws
              then fail $ "Keyword “" <> x <> "” used as identifier."
              else pure x

typeIdentifier :: Parser String
typeIdentifier = (lexemeN . try $ p) <?> "type identifier"
  where
    p =  (:)
            <$> C.upperChar
            <*> many (C.alphaNumChar <|> C.digitChar <|> oneOf ("'_" :: String))

typeVariable :: Parser String
typeVariable = (lexemeN . try $ p >>= check) <?> "type variable"
  where
    p = (:)
            <$> C.lowerChar
            <*> many (C.alphaNumChar <|> C.digitChar <|> oneOf ("'_" :: String))
    check x = if x `elem` kws
              then fail $ "Keyword “" <> x <> "” used as a type variable"
              else pure x

symbol :: Text -> Parser Text
symbol = lexemeN . L.symbol scN 

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

backticks :: Parser a -> Parser a
backticks = between (symbol "`") (symbol "`")

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

integer :: Parser Integer
integer = lexemeN L.decimal

float :: Parser Double
float = lexemeN L.float

string :: Text -> Parser Text
string = lexemeN . C.string

string' :: Text -> Parser Text
string' = C.string

string'' :: Parser String
string'' = lexemeN $ C.char '"' *> manyTill L.charLiteral (C.char '"')

char'' :: Parser Char
char'' = lexemeN $ between (C.char '\'') (C.char '\'') L.charLiteral

keyword :: Text -> Parser ()
keyword kw = lexemeN (string' kw *> notFollowedBy C.alphaNumChar) <?> show kw

opSymbol :: Parser String
opSymbol = (lexemeN . try) (some p <* notFollowedBy p) >>= check <?> "operator"
  where check x | x `elem` symbols = fail $ "Already existing operator “" <> x <> "”"
                -- | head x == ':'    = fail "An operator cannot start with “:” unless it is a type constructor"
                | otherwise        = pure x
        p = C.symbolChar <|> oneOf ("!#$%&.<=>?^~|@*/-:" :: String)

ctorSymbol :: Parser String
ctorSymbol = (lexemeN . try) p >>= check <?> "constructor"
  where check x = if x `elem` symbols
                  then fail $ "Already existing operator “" <> x <> "”"
                  else pure x
        
        p = do
            c1 <- C.char ':'
            cs <- many $ C.symbolChar <|> oneOf ("!#$%&.<=>?^~|@*/-:" :: String)
            pure (c1:cs)

indentGuard :: Ordering -> Pos -> Parser Pos
indentGuard = L.indentGuard scN

nonIndented :: Parser a -> Parser a
nonIndented = L.nonIndented scN
        
indented :: Pos -> Parser a -> Parser a
indented pos parser = do { indentGuard GT pos ; parser }

same :: Pos -> Parser a -> Parser a
same pos parser = do { indentGuard EQ pos ; parser }

less :: Pos -> Parser a -> Parser a
less pos parser = do { indentGuard LT pos ; parser }

sameOrIndented :: Pos -> Parser a -> Parser a
sameOrIndented pos parser = try (indented pos parser) <|> same pos parser

sameOrLess :: Pos -> Parser a -> Parser a
sameOrLess pos parser = try (less pos parser) <|> same pos parser

parseEither :: Parser a -> Parser b -> Parser (Either a b)
parseEither p1 p2 = Left <$> p1 <|> Right <$> p2

---------------------------------------------------------------------------------------------------------

kws :: [String] -- list of reserved words
kws =
    [ "infix"
    , "infixl"
    , "infixr"
    , "λ"
    , "match"
    , "with"
    , "data" 
    , "type" ]

symbols :: [String] -- list of reserved symbols
symbols =
        [ "->" , "→"
        , "-o" , "⊸"
        , "\\"
        , "="
        , "::" , "∷"
        , "," ]

builtins :: [String]
builtins =
    [ "Char"
    , "Integer"
    , "Double" ]