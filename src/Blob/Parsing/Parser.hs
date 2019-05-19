{-# LANGUAGE OverloadedStrings #-}

module Blob.Parsing.Parser
( program
, statement
) where

import Blob.Parsing.Types (Parser, Program(..), Statement(..), Expr(..), Associativity(..), Fixity(..), CustomOperator(..), ParseState(..), Scheme(..), CustomType(..), Type(..))
import Blob.Parsing.Lexer (lexeme, lexemeN, lineCmnt, blockCmnt, identifier, parens, opSymbol, symbol, integer, keyword, typeIdentifier, string, typeVariable, nonIndented, indented)
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
import Blob.PrettyPrinter.PrettyInference (pType)

program :: Parser Program
program = fmap Program $
    (many (hidden eol) *> eof $> []) <|> do
        x  <- do
            hidden $ many eol

            lineI <- indentLevel
            modify $ \st -> st { currentIndent = lineI }

            lexeme statement <?> "statement"
        xs <- many (do
                        some (hidden eol)

                        lineI <- indentLevel
                        modify $ \st -> st { currentIndent = lineI }

                        (lexeme statement <?> "statement") <|> (eof $> Empty)
            ) <* many (hidden eol) <* eof

        let statements = filter (/= Empty) (x:xs)
        pure statements

statement :: Parser Statement
statement = 
    nonIndented (lexeme (operator <|> try declaration <|> try definition <|> try sumType))
        <|> ((lineCmnt <|> blockCmnt) $> Empty)

declaration :: Parser Statement
declaration = do
    id' <- lexemeN ((identifier <|> parens opSymbol) <* (hidden (symbol "::") <|> symbol "∷")) <?> "identifier"
    Declaration id' <$> lexemeN type'

definition :: Parser Statement
definition = do
    pos  <- indentLevel
    id'  <- (identifier <|> parens opSymbol) <?> "identifier"
    args <- (many . indented pos) identifier <* symbol "="
    expr <- indented pos expression
    pure $ Definition id' (foldr ELam expr args)

operator :: Parser Statement
operator = do
    assocT     <- lexeme fixity' <?> "fixity"
    precedence <- integer <?> "precedence"

    if (precedence < 10) && (precedence >= 0)
    then do
        opT <- parens opSymbol

        addOperator $ CustomOperator (pack opT) (assocT precedence)

        pure $ OpDeclaration opT (assocT precedence)
    else fail $ "Invalid operator precedence `" ++ show precedence ++ "`. (should be >= 0 and <= 9)"

    where fixity' = Infix' L <$ keyword "infixl"
                <|> Infix' R <$ keyword "infixr"
                <|> Infix' N <$ keyword "infix"
                <|> Prefix'  <$ keyword "prefix"
                <|> Postfix' <$ keyword "postfix"

sumType :: Parser Statement
sumType = flip (<?>) "sum type" $ do
    name <- string "data" *> lexemeN typeIdentifier
    ts <- (many . lexemeN) typeVariable
    string "="
    ctor1 <- lexeme $ constructor name ts
    ctors <- many $ string "|" *> lexeme (constructor name ts)

    pure . TypeDeclaration name ts $ TSum (Map.fromList (ctor1:ctors))
  where constructor name ts = flip (<?>) "type constructor" $ do
            name' <- typeIdentifier
            type1 <- optional $ many atype'
            
            case type1 of
                Nothing -> pure (name', Scheme ts (foldl TApp (TId name) $ map TVar ts))
                Just cs -> pure (name', Scheme ts (foldr TFun (foldl TApp (TId name) $ map TVar ts) cs))
