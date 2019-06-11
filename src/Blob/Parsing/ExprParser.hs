{-# LANGUAGE OverloadedStrings #-}

module Blob.Parsing.ExprParser
( expression
) where

import Blob.Parsing.Types (Parser, Expr(..), Literal(..), ParseState(..), Pattern(..), Operator(..))
import qualified Data.MultiMap as MMap (elems)
import Control.Monad.State (get)
import Text.Megaparsec ((<?>), hidden, (<|>), try, many, some, optional, eof, empty, choice, option)
import Text.Megaparsec.Char (eol)
import Text.Megaparsec.Char.Lexer (indentLevel)
import Text.Megaparsec.Pos (Pos(..))
import Data.Functor ((<$), ($>))
import Blob.Parsing.Lexer 
import Control.Monad (MonadPlus(..))
import Debug.Trace

expression :: Parser Expr
expression = lexeme $ do
    st <- get
    let op = reverse . MMap.elems . operators $ st

    makeExprParser (lexeme term) op <?> "expression"

term :: Parser Expr
term = lambda'
   <|> match
   <|> EId <$> (identifier <|> try (parens opSymbol <?> "operator") <|> typeIdentifier)
   <|> ELit . LDec <$> try float
   <|> ELit . LInt <$> integer
   <|> ELit . LChr <$> char''
   <|> try tuple
   <|> list
   <|> ELit . LStr <$> string''
   <|> hidden (parens expression)

lambda' :: Parser Expr
lambda' = do
    params <- (symbol "λ" <|> hidden (symbol "\\")) *> many identifier <* (hidden (symbol "->") <|> symbol "→")
    expr <- expression

    pure $ foldr ELam expr params

tuple :: Parser Expr
tuple = lexemeN . parens $ do
    e1 <- expression
    e2 <- some (lexeme (string ",") *> expression)
    pure $ ETuple (e1 : e2)

list :: Parser Expr
list = lexemeN $
    (try (brackets (string "")) $> EId "[]")
    <|> brackets (do
        e1 <- expression
        es <- many (lexeme (string ",") *> expression)
        pure $ foldr (\exp1 exp2 -> EApp (EApp (EId ":") exp1) exp2) (EId "[]") (e1 : es))

match :: Parser Expr
match = do
    pos  <- indentLevel
    same pos $ keyword "match"
    expr <- sameOrIndented pos expression  
    sameOrIndented pos $ keyword "with"  
    pats <- parseCases pos

    pure $ EMatch expr pats
  where parseCases pos = some $ indented pos parseCase
        parseCase = do
            p      <- pattern'
            pos1   <- indentLevel
            sameOrIndented pos1 $ hidden (symbol "->") <|> symbol "→"
            e      <- indented pos1 expression
            pure (p, e)

pattern' :: Parser Pattern
pattern' = lexeme (makeExprParser patTerm patOps) <?> "pattern"

patTerm :: Parser Pattern
patTerm =   Wildcard <$  string "_"
        <|> PDec     <$> try float
        <|> PInt     <$> integer
        <|> PStr     <$> string''
        <|> PChr     <$> char''
        <|> PId      <$> identifier
        <|> PCtor    <$> typeIdentifier <*> many pattern'
        <|>              patternList
        <|>              parens pattern'
  where
    patternList :: Parser Pattern
    patternList =   (try . brackets) (string "") $> PCtor "[]" []
                <|> brackets (do { e1 <- pattern'
                                 ; es <- many (string "," *> pattern')
                                 ; pure $ foldr (\exp1 exp2 -> PCtor ":" [exp1, exp2]) (PCtor "[]" []) (e1:es) })

patOps :: [[Operator Parser Pattern]]
patOps = [ [ InfixR $ keySymbol ":" $> \e1 e2 -> PCtor ":" [e1, e2] ] ] -- prec == 5

---------------------------------------------------------------------------------------------------------
{- Control.Monad.Combinators.Expr rework for custom indentation -}

makeExprParser :: Parser a -> [[Operator Parser a]] -> Parser a
makeExprParser = foldl addPrecLevel

addPrecLevel :: Parser a -> [Operator Parser a] -> Parser a
addPrecLevel term ops = do
    pos <- indentLevel
    term' >>= \x -> choice [ras' pos x, las' pos x, nas' pos x, return x]
    where
        (ras, las, nas, prefix, postfix) = foldr splitOp ([],[],[],[],[]) ops
        term'    = pTerm (choice prefix) term (choice postfix)
        ras' pos = pInfixR pos (choice ras) term'
        las' pos = pInfixL pos (choice las) term' 
        nas' pos = pInfixN pos (choice nas) term'

pTerm :: Parser (a -> a) -> Parser a -> Parser (a -> a) -> Parser a
pTerm prefix term postfix = do
    pos  <- indentLevel
    pre  <- option id prefix
    x    <- sameOrIndented pos (try term)
    post <- option id (sameOrIndented pos postfix)
    return . post . pre $ x

pInfixN :: Pos -> Parser (a -> a -> a) -> Parser a -> a -> Parser a
pInfixN pos op p x = do
    pos' <- indentLevel 
    f <- sameOrIndented pos op
    y <- sameOrIndented pos' (try p)
    return $ f x y

pInfixL :: Pos -> Parser (a -> a -> a) -> Parser a -> a -> Parser a
pInfixL pos op p x = do
    pos' <- indentLevel 
    f <- sameOrIndented pos op
    y <- sameOrIndented pos' (try p)
    let r = f x y
    pInfixL pos' op p r <|> return r

pInfixR :: Pos -> Parser (a -> a -> a) -> Parser a -> a -> Parser a
pInfixR pos op p x = do
    pos' <- indentLevel
    f <- sameOrIndented pos op
    y <- sameOrIndented pos' (try p) >>= \r -> pInfixR pos' op p r <|> return r
    return $ f x y

type Batch a =
    ( [Parser (a -> a -> a)]
    , [Parser (a -> a -> a)]
    , [Parser (a -> a -> a)]
    , [Parser (a -> a)]
    , [Parser (a -> a)] )

splitOp :: Operator Parser a -> Batch a -> Batch a
splitOp (InfixR  op) (r, l, n, pre, post) = (op : r, l, n, pre, post)
splitOp (InfixL  op) (r, l, n, pre, post) = (r, op : l, n, pre, post)
splitOp (InfixN  op) (r, l, n, pre, post) = (r, l, op : n, pre, post)
splitOp (Prefix  op) (r, l, n, pre, post) = (r, l, n, op : pre, post)
splitOp (Postfix op) (r, l, n, pre, post) = (r, l, n, pre, op : post)
