{-# LANGUAGE OverloadedStrings #-}

module Blob.Parsing.TypeParser 
( type'
, atype'
) where

import Blob.Parsing.Types (Parser, Type(..), Expr(..), Literal(..))
import Blob.Parsing.Lexer (lexemeN, string, brackets, parens, typeVariable, typeIdentifier, braces, indented, sameOrIndented)
import Blob.Parsing.ExprParser (expression)
import Text.Megaparsec (optional, try, some, (<|>), (<?>), hidden, empty)
import Text.Megaparsec.Char.Lexer (indentLevel)
import Data.Functor (($>))

type' :: Parser Type
type' = lexemeN $ do
    pos         <- indentLevel
    firstId     <- (lexemeN . sameOrIndented pos) btype'
    multipleIds <- optional $ do
        pos'    <- indentLevel
        counter <- sameOrIndented pos $ (Just (ELit . LInt $ 1) <$ try (hidden (string "-o") <|> string "⊸") <?> "rounded arrow")
                                        <|> do
                                                lexemeN (hidden (string "->") <|> string "→") <?> "arrow"
                                                optional . try $ braces (lexemeN expression)

        pure (counter, indented pos' type')

    case multipleIds of
        Nothing                      -> pure firstId
        Just (Nothing, type'')       -> TArrow (ELit . LInt $ -1) firstId <$> type''
        Just (Just counter', type'') -> TArrow counter' firstId <$> type''

btype' :: Parser Type
btype' = do
    pos   <- indentLevel
    types <- some $ sameOrIndented pos atype'

    pure $ foldl1 TApp types

atype' :: Parser Type
atype' = 
    let tuple = do
            pos <- indentLevel
            lexemeN . parens $ do { t1 <- sameOrIndented pos type'
                                  ; sameOrIndented pos (string ",")
                                  ; tk <- some $ sameOrIndented pos type'
                                  ; pure $ TTuple (t1 : tk) }
        list = do
            pos <- indentLevel
            lexemeN $ (try (brackets (string "")) $> TId "[]")
                    <|> brackets (do
                        t <- sameOrIndented pos type'
                        pure $ TApp (TId "[]") t)
    in
        gtycon'
        <|> TVar <$> lexemeN typeVariable
        <|> try tuple
        <|> list
        <|> parens type'


gtycon' :: Parser Type
gtycon' = conid' <|> (try (string "()") $> TTuple [])

conid' :: Parser Type
conid' = TId <$> typeIdentifier
