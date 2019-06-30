{-# LANGUAGE LambdaCase, OverloadedStrings #-}

module Blob.Parsing.Parser where

import Blob.Parsing.Types
import Blob.Parsing.Lexer
import Text.Megaparsec as Mega
import Text.Megaparsec.Char hiding (string)
import qualified Data.Map as Map
import Data.Functor
import Text.Megaparsec.Char.Lexer hiding (lexeme, nonIndented)
import Blob.Parsing.TypeParser
import Blob.Parsing.ExprParser
import Blob.Parsing.Annotation
import Debug.Trace

parseProgram :: Parser Program
parseProgram = (lexemeN eof $> []) 
    <|> do
        s <- parseStatement
        ss <- many parseStatement <* eof

        pure (s:ss)

parseStatement :: Parser (Annotated Statement)
parseStatement = lexemeN . nonIndented $ parseOpDecl <|> parseSumType <|> parseTypeAlias <|> try parseDecl <|> parseDef

parseOpDecl :: Parser (Annotated Statement)
parseOpDecl = do
    init <- getSourcePos

    pos <- indentLevel
    fixity <- parseFix
    pos' <- indentLevel
    prec <- indented pos integer
    name <- indented pos' $ opSymbol <|> parens opSymbol

    end <- getSourcePos

    pure $ OpFixity name (fixity prec name :- Just (init, end)) :- Just (init, end)
  where parseFix =     keyword "infixr" $> Infix R
                   <|> keyword "infixl" $> Infix L
                   <|> keyword "infix"  $> Infix N

parseDecl :: Parser (Annotated Statement)
parseDecl = do
    init <- getSourcePos

    pos <- indentLevel
    name <- lexemeN $ identifier <|> parens opSymbol
    indented pos $ hidden (string "::") <|> string "∷"
    t <- indented pos type'

    end <- getSourcePos

    pure $ Declaration name t :- Just (init, end)

parseDef :: Parser (Annotated Statement)
parseDef = do
    init <- getSourcePos

    pos <- indentLevel
    name <- lexemeN $ identifier <|> parens opSymbol
    args <- lexemeN . many $ indented pos identifier
    indented pos $ string "="
    e <- indented pos parseExpression

    end <- getSourcePos

    pure $ Definition name args e :- Just (init, end)

parseSumType :: Parser (Annotated Statement)
parseSumType = flip (<?>) "sum type" $ do
    init <- getSourcePos

    pos   <- indentLevel
    keyword "data"
    pos'  <- indentLevel
    name  <- indented pos typeIdentifier
    ts    <- (many . sameOrIndented pos') typeVariable
    sameOrIndented pos' $ string "="

    ctorsInit <- getSourcePos
    ctor1 <- indented pos $ constructor name ts
    ctors <- many . indented pos $ string "|" *> constructor name ts

    end <- getSourcePos

    pure $ TypeDeclaration name ts (TSum (Map.fromList (ctor1:ctors)) :- Just (ctorsInit, end)) :- Just (init, end)
  where constructor name ts = flip (<?>) "type constructor" $ lexemeN $ do
            pos   <- indentLevel
            name' <- typeIdentifier -- <|> parens ctorSymbol
            (optional . many) (indented pos atype')
                >>= \case
                    Nothing -> pure (name', [])
                    Just cs -> pure (name', cs)

parseTypeAlias :: Parser (Annotated Statement)
parseTypeAlias = flip (<?>) "type alias" $ do
    init <- getSourcePos

    pos <- indentLevel
    keyword "type"
    pos' <- indentLevel
    name <- indented pos typeIdentifier
    ts <- (many . sameOrIndented pos') typeVariable
    sameOrIndented pos' $ string "="
    t <- indented pos type'

    end <- getSourcePos

    pure $ TypeDeclaration name ts (TAlias t :- getSpan t) :- Just (init, end)

runParser :: (Stream s, Show a) => Parsec e s a -> String -> s -> Either (ParseErrorBundle s e) a
runParser = Mega.runParser 