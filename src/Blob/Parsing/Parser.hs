{-# LANGUAGE OverloadedStrings #-}

module Blob.Parsing.Parser
( program
, statement
) where

import Blob.Parsing.Types (Parser, Program(..), Statement(..), Expr(..), Associativity(..), Fixity(..), CustomOperator(..), ParseState(..), Scheme(..), CustomType(..), Type(..))
import Blob.Parsing.Lexer
import Blob.Parsing.ExprParser (expression)
import Blob.Parsing.TypeParser (type', atype')
import Blob.Parsing.Defaults (addOperator)
import Text.Megaparsec (many, hidden, some, try, (<|>), (<?>), eof, optional)
import Text.Megaparsec.Char (eol)
import Text.Megaparsec.Char.Lexer as L (indentLevel, IndentOpt(..))
import Data.Functor(fmap, (<$>), ($>), (<$))
import Data.Text (pack)
import Control.Monad.State (lift, modify)
import qualified Data.Map as Map (fromList)
import Blob.Pretty.Inference (pType)

program :: Parser Program
program = fmap Program $
    (many (hidden eol) *> eof $> []) <|> do
        x  <- lexemeN statement <?> "statement"
        xs <- many (lexemeN statement <?> "statement") <* eof

        let statements = filter (/= Empty) (x:xs)
        pure statements

statement :: Parser Statement
statement = 
    nonIndented (operator <|> sumType <|> typeAlias <|> try declaration <|> definition)
        <|> ((lineCmnt <|> blockCmnt) $> Empty)

declaration :: Parser Statement
declaration = do
    pos   <- indentLevel
    id'   <- (identifier <|> parens opSymbol) <?> "identifier"
    pos'  <- indentLevel
    indented pos $ hidden (symbol "::") <|> symbol "∷"
    type_ <- indented pos' $ lexemeN type'
    pure $ Declaration id' type_

definition :: Parser Statement
definition = do
    pos  <- indentLevel
    id'  <- (identifier <|> parens opSymbol) <?> "identifier"
    args <- (many . indented pos) identifier <* symbol "="
    expr <- indented pos expression
    pure $ Definition id' (foldr ELam expr args)

operator :: Parser Statement
operator = do
    pos        <- indentLevel
    assocT     <- lexeme fixity' <?> "fixity"
    pos'       <- indentLevel
    precedence <- indented pos (integer <?> "precedence")

    if (precedence < 10) && (precedence >= 0)
    then do
        opT <- indented pos' $ parens opSymbol

        addOperator $ CustomOperator (pack opT) (assocT precedence)
        pure $ OpDeclaration opT (assocT precedence)
    else fail $ "Invalid operator precedence `" <> show precedence <> "`. (should be >= 0 and <= 9)"

    where fixity' = Infix' L <$ keyword "infixl"
                <|> Infix' R <$ keyword "infixr"
                <|> Infix' N <$ keyword "infix"
                <|> Prefix'  <$ keyword "prefix"
                <|> Postfix' <$ keyword "postfix"

sumType :: Parser Statement
sumType = flip (<?>) "sum type" $ do
    pos   <- indentLevel
    string "data"
    pos'  <- indentLevel
    name  <- indented pos typeIdentifier >>= check
    ts    <- (many . sameOrIndented pos') typeVariable
    sameOrIndented pos' $ string "="
    ctor1 <- lexeme . indented pos $ constructor name ts
    ctors <- many . indented pos $ string "|" *> lexeme (constructor name ts)

    pure . TypeDeclaration name ts . TSum $ Map.fromList (ctor1:ctors)
  where constructor name ts = flip (<?>) "type constructor" $ do
            pos   <- indentLevel
            name' <- typeIdentifier -- <|> parens ctorSymbol
            type1 <- optional . many . indented pos $ atype'
            
            case type1 of
                Nothing -> pure (name', Scheme ts (foldl TApp (TId name) $ map TVar ts))
                Just cs -> pure (name', Scheme ts (foldr TFun (foldl TApp (TId name) $ map TVar ts) cs))

        check x = if x `elem` builtins
                  then fail $ "Cannot alter definition of built-in type “" <> x <> "”."
                  else pure x

typeAlias :: Parser Statement
typeAlias = flip (<?>) "type alias" $ do
    pos <- indentLevel
    string "type"
    pos' <- indentLevel
    name <- indented pos typeIdentifier >>= check
    ts <- (many . sameOrIndented pos') typeVariable
    sameOrIndented pos' $ string "="
    t <- indented pos type'

    pure . TypeDeclaration name ts $ TAlias t
  where check x = if x `elem` builtins
                  then fail $ "Cannot alter definition of built-in type “" <> x <> "”."
                  else pure x
