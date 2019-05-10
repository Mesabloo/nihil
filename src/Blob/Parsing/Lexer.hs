{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Blob.Parsing.Lexer where

import Text.Megaparsec (hidden, some, many, skipMany, skipSome, oneOf, try, (<?>), (<|>), between, manyTill, notFollowedBy, eof)
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Pos (Pos)
import Blob.Parsing.Types (Parser, ParseState(..))
import Data.Text (Text, pack)
import Control.Monad.State (get, lift, modify)
import Data.Functor (($>))

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
            <$> (C.lowerChar <|> oneOf ("'_" :: String))
            <*> many (C.alphaNumChar <|> C.digitChar <|> oneOf ("'_" :: String))
    check x = if x `elem` kws
              then fail $ "Keyword “" ++ x ++ "” used as identifier."
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
            <$> C.lowerChar
            <*> many (C.alphaNumChar <|> C.digitChar <|> oneOf ("'_" :: String))
    check x = if x `elem` kws
              then fail $ "Keyword “" ++ x ++ "” used as a type variable"
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
opSymbol = (lexeme (some C.symbolChar) >>= check) <?> "operator"
  where check x = if x `elem` symbols
                  then fail $ "Already existing operator “" ++ x ++ "”"
                  else pure x

indentGuard :: Ordering -> Pos -> Parser Pos
indentGuard = L.indentGuard (skipSome (oneOf (" \t" :: String) <|> (' ' <$ C.eol)))

indented :: Parser a -> Parser a
indented p = do
    env <- get
    let curIndent = currentIndent env
    newPos <- indentGuard GT curIndent
    lift . modify $ \st -> st { currentIndent = newPos }

    p
        
block :: Parser a -> Parser [a]
block p = do
    e1  <- indented p
    curIndent <- currentIndent <$> get

    es <- many $ do
        indentGuard EQ curIndent
        p

    pure (e1:es)

block1 :: Parser a -> Parser [a]
block1 p = do
    e1  <- indented p
    curIndent <- currentIndent <$> get

    es <- some $ do
        indentGuard EQ curIndent
        p

    pure (e1:es)

---------------------------------------------------------------------------------------------------------

kws :: [String] -- list of reserved words
kws =
    [ "prefix"
    , "postfix"
    , "infix"
    , "infixl"
    , "infixr"
    , "λ"
    , "match"
    , "with"
    , "data"
    ]

symbols :: [String] -- list of reserved symbols
symbols =
        [ "->"
        , "→"
        , "-o"
        , "⊸"
        , "\\"
        , "="
        , ":" ]