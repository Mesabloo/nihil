{-# LANGUAGE OverloadedStrings #-}

module Blob.Parsing.ExprParser where

import Blob.Parsing.Types
import Text.Megaparsec hiding (match)
import Blob.Parsing.Lexer
import Text.Megaparsec.Char hiding (string)
import Data.Functor
import Text.Megaparsec.Char.Lexer hiding (float, symbol, lexeme)
import Blob.Parsing.Annotation

parseExpression :: Parser (Annotated Expr)
parseExpression = do
    init <- getSourcePos

    pos <- indentLevel
    a <- lexemeN $ some (sameOrIndented pos atom)

    end <- getSourcePos

    pure $ a :- Just (init, end)

atom :: Parser (Annotated Atom)
atom = operator <|> expr
  where
    expr = try app <|> exprNoApp

exprNoApp :: Parser (Annotated Atom)
exprNoApp = do
    init <- getSourcePos
    a <- hole
        <|> lambda'
        <|> match
        <|> AId <$> (identifier <|> try (parens opSymbol <?> "operator") <|> typeIdentifier)
        <|> ALit . LDec <$> try float
        <|> ALit . LInt <$> integer
        <|> ALit . LChr <$> char''
        <|> try tuple
        <|> list
        <|> ALit . LStr <$> string''
        <|> AParens <$> hidden (parens parseExpression)
    end <- getSourcePos

    pure $ a :- Just (init, end)

app :: Parser (Annotated Atom)
app = do
    init <- getSourcePos
    pos <- indentLevel
    a1 <- exprNoApp
    as <- some . indented pos $ exprNoApp
    end <- getSourcePos

    pure $ getAnnotated (foldl (\e1 e2 -> AApp e1 e2 :- Nothing) a1 as) :- Just (init, end)

operator :: Parser (Annotated Atom)
operator = do
    init <- getSourcePos
    op <- opSymbol
    end <- getSourcePos

    pure $ AOperator op :- Just (init, end)

hole :: Parser Atom
hole = lexemeN $ do
    some (char '_')
    pure AHole

lambda' :: Parser Atom
lambda' = do
    pos <- indentLevel
    symbol "λ" <|> hidden (symbol "\\")
    params <- many $ indented pos identifier
    pos' <- indentLevel
    indented pos $ hidden (symbol "->") <|> symbol "→"

    ALambda params <$> indented pos' parseExpression

tuple :: Parser Atom
tuple = lexemeN . parens $ do
    e1 <- parseExpression
    e2 <- some (lexeme (string ",") *> parseExpression)
    pure $ ATuple (e1 : e2)

list :: Parser Atom
list = brackets (string "" $> AList []
                 <|> do
                        e1 <- parseExpression
                        es <- many (string "," *> parseExpression)
                        
                        pure $ AList (e1 : es))

-- patterns

match :: Parser Atom
match = do
    pos  <- indentLevel
    same pos $ keyword "match"
    expr <- sameOrIndented pos parseExpression  
    sameOrIndented pos $ keyword "with"  
    pats <- parseCases pos

    pure $ AMatch expr pats
  where parseCases pos = some $ indented pos parseCase
        parseCase = do
            p      <- pattern'
            pos1   <- indentLevel
            sameOrIndented pos1 $ hidden (symbol "->") <|> symbol "→"
            e      <- indented pos1 parseExpression
            pure (p, e)

pattern' :: Parser [Annotated Pattern]
pattern' = lexemeN $ some (patTerm <|> try patOperator)

patOperator :: Parser (Annotated Pattern)
patOperator = do
    init <- getSourcePos
    p <- opSymbol
    end <- getSourcePos

    pure $ POperator p :- Just (init, end)

patTerm :: Parser (Annotated Pattern)
patTerm = do
    init <- getSourcePos
    p <-    PHole       <$  hole
        <|> PLit . LDec <$> try float
        <|> PLit . LInt <$> integer
        <|> PLit . LChr <$> char''
        <|> PId         <$> identifier
        <|> PCtor       <$> typeIdentifier <*> many pattern'
        <|>                 try patternTuple
        <|>                 patternList
        <|> PLit . LStr <$> string''
        <|> PParens     <$> parens pattern'
    end <- getSourcePos

    pure $ p :- Just (init, end)
  where
    patternList :: Parser Pattern
    patternList =   brackets (string "" $> PList []
                            <|> do
                                    e1 <- pattern'
                                    es <- many (string "," *> pattern')

                                    pure $ PList (e1 : es))

    patternTuple :: Parser Pattern
    patternTuple = parens $ do
        e1 <- pattern'
        es <- some (string "," *> pattern')
        pure $ PTuple (e1:es)

-- patOps :: [[Operator Parser Pattern]]
-- patOps = [ [ InfixR $ keySymbol ":" $> \e1 e2 -> PCtor ":" [e1, e2] ] ] -- prec == 5