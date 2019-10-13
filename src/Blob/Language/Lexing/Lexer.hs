{-# LANGUAGE BlockArguments, OverloadedStrings #-}

-- | This module contains all the functions used for lexing a source file.
module Blob.Language.Lexing.Lexer where

import Text.Megaparsec hiding (Token, tokens)
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L
import Blob.Language.Lexing.Types
import Control.Monad.State
import Control.Applicative (liftA2)
import qualified Data.Char as Ch
import qualified Data.Text as Text
import Data.Functor
import Data.Void
import Control.Lens

-- | The main function, used to transform a source file into a list of tokens.
tokens :: Parser [Token]
tokens = lexeme (indent *> optional (try blockCmnt <|> lineCmnt) *> many tks) <* eof
  where tks = lexeme $ keyword <|> stringL <|> integerL <|> floatL <|> charL <|> symbol <|> identifier <|> identifier' <|> eolI <|> wildcard

-- | This function is used to calculate the indentation level of each line when EOL is encountered.
eolI :: Parser Token
eolI = do
    (pInit, pEnd, _) <- getPositionInSource $ do
        satisfy isEndOfLine
        indent

    i <- use currentIndent
    pure (i, SourceSpan pInit pEnd, Nothing)

-- | The internal indentation calculator used in @eolI@.
indent :: Parser ()
indent = do
    indent' <- length <$> space'
    currentIndent .= indent'

-- | This function parses a keyword
keyword :: Parser Token
keyword = lexeme $ do
    i <- use currentIndent

    (pInit, pEnd, kw) <- getPositionInSource $
        LKeyword <$> (choice (C.string <$> kwords) <* notFollowedBy (satisfy $ liftA2 (&&) Ch.isPrint (not . Ch.isSpace)))

    pure (i, SourceSpan pInit pEnd, Just kw)

-- | This function parses a string.
--
-- A string has the following format:
--
-- > string ::= '"', anyCharButEOL, '"' ;
stringL :: Parser Token
stringL = lexeme $ do
    i <- use currentIndent
    (pInit, pEnd, s) <- getPositionInSource $
        LString <$> (C.char '"' *> takeWhileP (Just "unescaped character") (/= '"') <* C.char '"')

    pure (i, SourceSpan pInit pEnd, Just s)

-- | This function parses an integer.
--
-- A integer currently has the following format: (it will be changed later to handle different signednesses)
--
-- > integer ::= { decimalNumber } ;
integerL :: Parser Token
integerL = lexeme $ do
    i <- use currentIndent
    (pInit, pEnd, int) <- getPositionInSource $
        L.decimal <&> LInteger

    pure (i, SourceSpan pInit pEnd, Just int)

-- | This function parses a floating point number.
--
-- A float has the following format:
--
-- > float ::= [ '-' ], { decimalNumber }, ',', { decimalNumber } ;
floatL :: Parser Token
floatL = lexeme $ do
    i <- use currentIndent
    (pInit, pEnd, flo) <- getPositionInSource $
        L.float <&> LFloat

    pure (i, SourceSpan pInit pEnd, Just flo)

-- | This function parses a character.
--
-- A character has the following format:
--
-- > character ::= '\'', anyCharButEOL, '\'' ;
charL :: Parser Token
charL = lexeme $ do
    i <- use currentIndent
    (pInit, pEnd, chr) <- getPositionInSource $
        LChar <$> (C.char '\'' *> anySingle <* C.char '\'')

    pure (i, SourceSpan pInit pEnd, Just chr)

-- | This function parses a symbol.
--
-- A symbol has the following format:
--
-- > symbol ::= '-o' | aSpecialCharacter | { symbolCharacter } ;
-- > aSpecialCharacter ::= '(' | ')' | '[' | ']' | '{' | '}' | ',' | ';' | '\\' | '→' | '⊸' | 'λ' | '⇒' | '∷'
-- > symbolCharacter ::= '!' | '#' | '$' | '%' | '&' | '.' | '<' | '=' | '>' | '?' | '^' | '~' | '|' | '@' | '*' | '/' | '-' | ':' ;
symbol :: Parser Token
symbol = do
    i <- use currentIndent
    (pInit, pEnd, s) <- getPositionInSource $
        (LSymbol <$> lexeme (C.string "-o" <* notFollowedBy (satisfy $ liftA2 (&&) Ch.isPrint (not . Ch.isSpace))))
        <|> (LSymbol . Text.pack . (: []) <$> lexeme (oneOf ("()[]{},;\\→⊸λ⇒∷" :: String)))
        <|> (LSymbol . Text.pack <$> lexeme (some $ C.symbolChar <|> oneOf ("!#$%&.<=>?^~|@*/-:" :: String)) <?> "symbol")

    pure (i, SourceSpan pInit pEnd, Just s)

-- | This function parses a lowercased identifier.
--
-- An identifier has the following format:
--
-- > identifier ::= lowerChar, { alphaNumChar | '_' | '\'' } ;
identifier :: Parser Token
identifier = lexeme $ do
    i <- use currentIndent
    (pInit, pEnd, ident) <- getPositionInSource $
        LLowIdentifier <$> (p >>= check)

    pure (i, SourceSpan pInit pEnd, Just ident)
  where p = Text.pack <$> ((:) <$> C.lowerChar <*> many (C.alphaNumChar <|> satisfy (liftA2 (||) (== '_') (== '\''))))
        check x | x `elem` kwords = fail ("“" <> Text.unpack x <> "” is a keyword and thus cannot be used as an identifier")
                | otherwise       = pure x

-- | This function parses an uppercased identifier.
--
-- An identifier has the following format:
--
-- > identifier' ::= upperChar, { alphaNumChar | '_' | '\'' } ;
identifier' :: Parser Token
identifier' = lexeme $ do
    i <- use currentIndent
    (pInit, pEnd, ident) <- getPositionInSource $
        LUpIdentifier . Text.pack <$> ((:) <$> C.upperChar <*> many (C.alphaNumChar <|> satisfy (liftA2 (||) (== '_') (== '\''))))

    pure (i, SourceSpan pInit pEnd, Just ident)

-- | This function parses a wildcard pattern.
--
-- A wildcard has the following format:
--
-- > wildcard ::= { '_' } ;
wildcard :: Parser Token
wildcard = lexeme $ do
    i <- use currentIndent
    (pInit, pEnd, w) <- getPositionInSource $
        LWildcard <$ some (C.char '_')

    pure (i, SourceSpan pInit pEnd, Just w)

--------------------------------------------------------------------------------------------------

-- | A useful function for skipping whitespaces and comments.
lexeme :: Parser a -> Parser a
lexeme = L.lexeme (L.space (void space1') lineCmnt blockCmnt)

-- | A parser for skipping line comments.
--
-- Line comments begin with @--@.
lineCmnt :: Parser ()
lineCmnt = L.skipLineComment "--"

-- | A parser for skipping block comments.
--
-- Block comments are formatted like that:
--
-- > blockCmnt ::= '{-', { anyChar } , '-}' ;
blockCmnt :: Parser ()
blockCmnt = L.skipBlockComment "{-" "-}"

-- | A parser for skipping many spaces (0 or more).
space' :: Parser [Char]
space' = many (satisfy isSpace)

-- | A parser for skipping some spaces (1 or more).
space1' :: Parser [Char]
space1' = some (satisfy isSpace)

-- | A simple predicate checking whether a character is a space or not.
isSpace :: Char -> Bool
isSpace c =
    let code = Ch.ord c
    in code == 9 || code == 32 || code == 160 || code == 8200 || code == 8201 || code == 8202

-- | A simple predicate checking whether a character is an end of line character or not.
isEndOfLine :: Char -> Bool
isEndOfLine c =
    let code = Ch.ord c
    in code == 10 || code == 11 || code == 12 || code == 13 || code == 133 || code == 8232 || code == 8233

-- | A simple wrapper around the 'C.string' function.
string :: Text.Text -> Parser Text.Text
string = lexeme . C.string

-- | A function used to create tokens
--
-- The 'Parser' taken as a parameter is the one used for the creation of the token.
-- It most likely returns a 'Blob.Language.Lexing.Lexeme', but not necessarily.
getPositionInSource :: Parser a -> Parser (SourcePos, SourcePos, a)
getPositionInSource p = do
    pInit <- getSourcePos
    res <- p
    pEnd <- getSourcePos

    pure (pInit, pEnd, res)
-----------------------------------------------------------------------------------------------------------

-- | The list of keywords of the language.
kwords :: [Text.Text]
kwords = [ "match", "with", "data", "type", "infixl", "infixr", "infix", "where", "let", "in" ]

-----------------------------------------------------------------------------------------------------------

-- | A simple wrapper function for running the lexer with some text as input.
runLexer :: Text.Text -> String -> Either (ParseErrorBundle Text.Text Void) [Token]
runLexer content fileName =
    evalState (runParserT tokens fileName content) initLexState