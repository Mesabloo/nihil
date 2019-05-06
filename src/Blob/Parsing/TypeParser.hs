{-# LANGUAGE OverloadedStrings #-}

module Blob.Parsing.TypeParser 
( type'
) where

import Blob.Parsing.Types (Parser, Type(..), Expr(..), Literal(..))
import Blob.Parsing.Lexer (lexeme, string, brackets, parens, typeVariable, typeIdentifier)
import Blob.Parsing.ExprParser (expression)
import Text.Megaparsec (optional, try, some, (<|>), (<?>))
import Data.Functor (($>))

type' :: Parser Type
type' = lexeme $ do
    firstId     <- lexeme btype'
    multipleIds <- optional $ do
        lexeme (string "->" <|> string "→") <?> "arrow"
        counter <- optional $ brackets (lexeme expression)
        pure (counter, type')

    case multipleIds of
        Nothing                      -> pure firstId
        Just (Nothing, type'')       -> TArrow (ELit $ LInt 0) firstId <$> type''
        Just (Just counter', type'') -> TArrow counter' firstId <$> type''

btype' :: Parser Type
btype' = lexeme $ do
    t <- atype'
    types <- optional (try btype')
    case types of
        Nothing -> pure t
        Just ty -> pure $ TApp t ty 

atype' :: Parser Type
atype' = 
    let tuple = lexeme . parens $ do {
        t1 <- lexeme type' ;
        tk <- some (lexeme (string ",") *> lexeme type') ;
        pure $ TTuple (t1 : tk)
    }
        list = lexeme . brackets $ do {
        TList <$> lexeme type'
    }
    in
        gtycon'
        <|> TVar <$> lexeme typeVariable
        <|> try tuple
        <|> try list
        <|> try (parens type')


gtycon' :: Parser Type
gtycon' = try conid' <|> (try (string "()") $> TTuple [])

conid' :: Parser Type
conid' = TId <$> try typeIdentifier