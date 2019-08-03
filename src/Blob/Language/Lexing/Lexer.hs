{-# LANGUAGE BlockArguments, OverloadedStrings #-}

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
import Text.PrettyPrint.Leijen (Doc)
import Data.Void

tokens :: Parser [Token]
tokens = lexeme $ indent *> many tks <* eof
  where tks = lexeme $ keyword <|> stringL <|> integerL <|> floatL <|> charL <|> symbol <|> identifier <|> identifier' <|> eolI <|> wildcard

eolI :: Parser Token
eolI = do
    (pInit, pEnd, _) <- getPositionInSource $ do
        satisfy isEndOfLine
        indent

    i <- gets currentIndent
    pure (i, SourceSpan pInit pEnd, Nothing)

indent :: Parser ()
indent = do
    indent <- length <$> space'
    put $ LexState indent

keyword :: Parser Token
keyword = lexeme $ do
    i <- gets currentIndent 

    (pInit, pEnd, kw) <- getPositionInSource $
        LKeyword <$> (choice (C.string <$> kwords) <* notFollowedBy (satisfy $ liftA2 (&&) Ch.isPrint (not . Ch.isSpace)))

    pure (i, SourceSpan pInit pEnd, Just kw)

stringL :: Parser Token
stringL = lexeme $ do
    i <- gets currentIndent
    (pInit, pEnd, s) <- getPositionInSource $
        LString <$> (C.char '"' *> takeWhileP (Just "unescaped character") (/= '"') <* C.char '"')

    pure (i, SourceSpan pInit pEnd, Just s)

integerL :: Parser Token
integerL = lexeme $ do
    i <- gets currentIndent
    (pInit, pEnd, int) <- getPositionInSource $
        L.decimal <&> LInteger

    pure (i, SourceSpan pInit pEnd, Just int)

floatL :: Parser Token
floatL = lexeme $ do
    i <- gets currentIndent
    (pInit, pEnd, flo) <- getPositionInSource $
        L.float <&> LFloat

    pure (i, SourceSpan pInit pEnd, Just flo)

charL :: Parser Token
charL = lexeme $ do
    i <- gets currentIndent
    (pInit, pEnd, chr) <- getPositionInSource $
        LChar <$> (C.char '\'' *> anySingle <* C.char '\'')

    pure (i, SourceSpan pInit pEnd, Just chr)

symbol :: Parser Token
symbol = do
    i <- gets currentIndent
    (pInit, pEnd, s) <- getPositionInSource $
        (LSymbol <$> lexeme (string "-o" <* notFollowedBy (satisfy $ liftA2 (&&) Ch.isPrint (not . Ch.isSpace))))
        <|> (LSymbol . Text.pack . (: []) <$> lexeme (oneOf ("()[]{},;\\→⊸λ⇒∷" :: String)))
        <|> (LSymbol . Text.pack <$> lexeme (some $ C.symbolChar <|> oneOf ("!#$%&.<=>?^~|@*/-:" :: String)) <?> "symbol")

    pure (i, SourceSpan pInit pEnd, Just s)

identifier :: Parser Token
identifier = lexeme $ do
    i <- gets currentIndent
    (pInit, pEnd, ident) <- getPositionInSource $
        LLowIdentifier <$> (p >>= check)

    pure (i, SourceSpan pInit pEnd, Just ident)
  where p = Text.pack <$> ((:) <$> C.lowerChar <*> many (C.alphaNumChar <|> satisfy (liftA2 (||) (== '_') (== '\''))))
        check x | x `elem` kwords = fail ("“" <> Text.unpack x <> "” is a keyword and thus cannot be used as an identifier")
                | otherwise       = pure x

identifier' :: Parser Token
identifier' = lexeme $ do
    i <- gets currentIndent
    (pInit, pEnd, ident) <- getPositionInSource $
        LUpIdentifier . Text.pack <$> ((:) <$> C.upperChar <*> many (C.alphaNumChar <|> satisfy (liftA2 (||) (== '_') (== '\''))))

    pure (i, SourceSpan pInit pEnd, Just ident)

wildcard :: Parser Token
wildcard = lexeme $ do
    i <- gets currentIndent
    (pInit, pEnd, w) <- getPositionInSource $
        LWildcard <$ some (C.char '_')

    pure (i, SourceSpan pInit pEnd, Just w)

--------------------------------------------------------------------------------------------------

lexeme :: Parser a -> Parser a
lexeme = L.lexeme (L.space (void space1') lineCmnt blockCmnt)
  where lineCmnt = void $ C.string "--" *> takeWhileP (Just "comment character") (not . isEndOfLine)
        blockCmnt = empty

space' :: Parser [Char]
space' = many (satisfy isSpace)

space1' :: Parser [Char]
space1' = some (satisfy isSpace)

isSpace :: Char -> Bool
isSpace c =
    let code = Ch.ord c
    in code == 9 || code == 32 || code == 160 || code == 8200 || code == 8201 || code == 8202

isEndOfLine :: Char -> Bool
isEndOfLine c =
    let code = Ch.ord c
    in code == 10 || code == 11 || code == 12 || code == 13 || code == 133 || code == 8232 || code == 8233

string :: Text.Text -> Parser Text.Text
string = lexeme . C.string

getPositionInSource :: Parser a -> Parser (SourcePos, SourcePos, a)
getPositionInSource p = do
    pInit <- getSourcePos
    res <- p
    pEnd <- getSourcePos

    pure (pInit, pEnd, res)
-----------------------------------------------------------------------------------------------------------

kwords :: [Text.Text]
kwords = [ "match", "with", "data", "type", "infixl", "infixr", "infix", "where" ]

-----------------------------------------------------------------------------------------------------------

runLexer :: Text.Text -> String -> Either (ParseErrorBundle Text.Text Void) [Token]
runLexer content fileName = 
    evalState (runParserT tokens fileName content) initLexState