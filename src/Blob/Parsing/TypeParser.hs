{-# LANGUAGE OverloadedStrings #-}

module Blob.Parsing.TypeParser 
( type'
, atype'
) where

import Blob.Parsing.Types (Parser, Type(..), Expr(..), Literal(..))
import Blob.Parsing.Lexer (lexeme, string, brackets, parens, typeVariable, typeIdentifier, braces)
import Blob.Parsing.ExprParser (expression)
import Text.Megaparsec (optional, try, some, (<|>), (<?>), hidden, empty)
import Data.Functor (($>))

type' :: Parser Type
type' = lexeme $ do
    firstId     <- lexeme btype'
    multipleIds <- optional $ do
        -- counter <- optional $ ((ELit . LInt $ 1) <$ try (hidden (string "-o") <|> string "⊸") <?> "arrow")
        --            <|> do
        --                lexeme (hidden (string "->") <|> string "→") <?> "arrow"
        --                counter <- optional $ braces (lexeme expression)
        --                pure $ fromMaybe (ELit . LInt $ -1) counter
        counter <- (Just (ELit . LInt $ 1) <$ try (hidden (string "-o") <|> string "⊸") <?> "rounded arrow")
                   <|> do
                        lexeme (hidden (string "->") <|> string "→") <?> "arrow"
                        optional . try $ braces (lexeme expression)

        pure (counter, type')

    case multipleIds of
        Nothing                      -> pure firstId
        Just (Nothing, type'')       -> TArrow (ELit . LInt $ -1) firstId <$> type''
        Just (Just counter', type'') -> TArrow counter' firstId <$> type''

btype' :: Parser Type
btype' = foldl1 TApp <$> some atype'

atype' :: Parser Type
atype' = 
    let tuple = lexeme . parens $ do {
        t1 <- lexeme type' ;
        tk <- some (lexeme (string ",") *> lexeme type') ;
        pure $ TTuple (t1 : tk)
    }
        list = lexeme ((brackets (string "") $> TList)
                <|> brackets (do
                    t <- lexeme type'
                    pure $ TApp TList t))
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
