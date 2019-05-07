{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Blob.Parsing.Lexer where

import Text.Megaparsec (hidden, some, many, skipMany, skipSome, oneOf, try, (<?>), (<|>), between, manyTill, notFollowedBy, eof)
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L
import Blob.Parsing.Types (Parser)
import Data.Text (Text, pack)

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

space' :: Parser ()
space' = skipMany $ oneOf (" \t" :: String)

sc :: Parser ()
sc = L.space space1' lineCmnt blockCmnt

lineCmnt :: Parser ()
lineCmnt  = hidden $ L.skipLineComment (pack "--")

blockCmnt :: Parser ()
blockCmnt = lexeme . hidden $ L.skipBlockComment (pack "{-") (pack "-}")

space1' :: Parser ()
space1' = skipSome $ oneOf (" \t" :: String)

identifier :: Parser String
identifier = (lexeme . try $ p >>= check) <?> "identifier"
  where
    p = (:)
            <$> (C.letterChar <|> oneOf ("'_" :: String))
            <*> many (C.alphaNumChar <|> C.digitChar <|> oneOf ("'_" :: String))
    check x = if x `elem` kws
              then fail $ "Keyword “" ++ show x ++ "” used as identifier."
              else pure x

typeIdentifier :: Parser String
typeIdentifier = lexeme . try $ do
    first <- C.upperChar
    second <- many (C.alphaNumChar <|> C.digitChar <|> oneOf ("'_" :: String))
    pure $ first:second

typeVariable :: Parser String
typeVariable = (lexeme . try $ p >>= check) <?> "type variable"
  where
    p = (:)
            <$> C.letterChar
            <*> many (C.alphaNumChar <|> C.digitChar <|> oneOf ("'_" :: String))
    check x = if x `elem` kws
              then fail $ "Keyword “" ++ show x ++ "” used as a type variable"
              else pure x

symbol :: Text -> Parser Text
symbol s = lexeme . try $ L.symbol sc s

parens :: Parser a -> Parser a
parens p = lexeme . try $ between (symbol "(") (symbol ")") p

brackets :: Parser a -> Parser a
brackets p = lexeme . try $ between (symbol "[") (symbol "]") p

backticks :: Parser a -> Parser a
backticks p = lexeme . try $ between (string "`") (string "`") p

braces :: Parser a -> Parser a
braces p = lexeme . try $ between (string "{") (string "}") p

integer :: Parser Integer
integer = lexeme L.decimal

float :: Parser Double
float = lexeme L.float

string :: Text -> Parser Text
string s = lexeme $ C.string s

string' :: Text -> Parser Text
string' = C.string

string'' :: Parser String
string'' = lexeme $ C.char '"' *> manyTill L.charLiteral (C.char '"')

keyword :: Text -> Parser ()
keyword kw = lexeme (string' kw *> notFollowedBy C.alphaNumChar) <?> show kw

opSymbol :: Parser String
opSymbol = lexeme (some C.symbolChar) <?> "operator"


---------------------------------------------------------------------------------------------------------

kws :: [String] -- list of reserved words
kws =
    [ "prefix"
    , "postfix"
    , "infix"
    , "infixl"
    , "infixr"
    , "λ"
    ]