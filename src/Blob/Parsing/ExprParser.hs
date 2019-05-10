{-# LANGUAGE OverloadedStrings #-}

module Blob.Parsing.ExprParser
( expression
) where

import Blob.Parsing.Types (Parser, Expr(..), Literal(..), ParseState(..), Pattern(..))
import qualified Data.MultiMap as MMap (elems)
import Control.Monad.Combinators.Expr (Operator, makeExprParser, Operator(..))
import Control.Monad.State (get)
import Text.Megaparsec ((<?>), hidden, (<|>), try, many, some, optional)
import Text.Megaparsec.Char (eol)
import Data.Functor ((<$), ($>))
import Blob.Parsing.Lexer (lexeme, float, integer, identifier, opSymbol, parens, string'', space', symbol, brackets, string', string, keyword)

expression :: Parser Expr
expression = lexeme $ do
    st <- get
    let op = reverse . MMap.elems . operators $ st
    makeExprParser (lexeme term) (table op) <?> "expression"

term :: Parser Expr
term = try lambda'
--   <|> match
   <|> EId <$> (identifier <|> try (parens opSymbol <?> "operator"))
   <|> ELit . LDec <$> try float
   <|> ELit . LInt <$> try integer
   <|> try tuple
   <|> try list
   <|> ELit . LStr <$> try string''
   <|> hidden (parens expression)

table :: [[Operator Parser Expr]] -> [[Operator Parser Expr]]
table = ([ InfixL $ EApp <$ space' ] :)

-- infixL, infixR, infixN :: (Expr -> Expr -> Expr) -> Parser Text -> Operator Parser Expr
-- infixL op sym = InfixL (op <$ sym)
-- infixR op sym = InfixR (op <$ sym)
-- infixN op sym = InfixN (op <$ sym)

-- prefix, postfix :: (Expr -> Expr) -> Parser Text -> Operator Parser Expr
-- prefix op sym = Prefix (op <$ sym)
-- postfix op sym = Postfix (op <$ sym)

-- anyOp :: Operator Parser Expr
-- anyOp = 
--     let name = (try (parens opSymbol <?> "operator") <|> try (backticks identifier <?> "infix function") >>= check)
--     in infixL (\exp1 exp2 -> pure App (App (Id <$> name) exp1) exp2) (lexeme $ string name)
--   where check x = if x `elem` kws
--                   then fail $ "Keyword “" ++ x ++ "” used as function application."
--                   else pure x

lambda' :: Parser Expr
lambda' = do
    params <- (symbol "λ" <|> hidden (symbol "\\")) *> many identifier <* (hidden (symbol "->") <|> symbol "→")
    expr <- expression

    pure $ foldr ELam expr params

tuple :: Parser Expr
tuple = lexeme . parens $ do
    e1 <- expression
    e2 <- some (lexeme (string ",") *> expression)
    pure $ ETuple (e1 : e2)

list :: Parser Expr
list = lexeme . brackets $ do
    e1 <- expression
    e2 <- some (lexeme (string ",") *> expression)
    pure $ EList (e1 : e2)

-- match :: Parser Expr
-- match = do
--     toMatch <- keyword "match" *> expression <* keyword "with"
--     optional $ lexeme eol
--     patternsAndVals <- some (do
--         p <- lexeme pattern'
--         lexeme (hidden (string "->") <|> string "→")
--         v <- expression
--         eol
--         pure (p, v)) <?> "patterns"

--     pure $ EMatch toMatch patternsAndVals 

-- pattern' :: Parser Pattern
-- pattern' = (string' "_" $> Wildcard)
--           <|> (PDec <$> try float)
--           <|> (PInt <$> try integer)
--           <|> (PStr <$> try string'')