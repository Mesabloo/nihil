{-# LANGUAGE OverloadedStrings #-}

module Blob.Parsing.TypeParser where

import Blob.Parsing.Types
import Blob.Parsing.Lexer
import Text.Megaparsec
import Blob.Parsing.ExprParser
import Text.Megaparsec.Char.Lexer
import Data.Functor
import Blob.Parsing.Annotation

type' :: Parser (Annotated Type)
type' = lexemeN $ do
    init <- getSourcePos

    pos         <- indentLevel
    firstId     <- (lexemeN . sameOrIndented pos) btype'
    multipleIds <- optional $ do
        pos'    <- indentLevel
        counter <- sameOrIndented pos $ (Just ([(ALit . LInt $ 1) :- Nothing] :- Nothing) <$ try (hidden (string "-o") <|> string "⊸") <?> "rounded arrow")
                                        <|> do
                                                lexemeN (hidden (string "->") <|> string "→") <?> "arrow"
                                                optional . try $ braces (lexemeN parseExpression)

        pure (counter, indented pos' type')

    case multipleIds of
        Nothing                      -> pure firstId
        Just (Nothing, type'')       -> do
            t <- type''
            end <- getSourcePos

            pure $ TFun firstId t :- Just (init, end)
        Just (Just counter', type'') -> do
            t <- type''
            end <- getSourcePos

            pure $ TArrow counter' firstId t :- Just (init, end)

btype' :: Parser (Annotated Type)
btype' = do
    init <- getSourcePos

    pos   <- indentLevel
    types <- some $ sameOrIndented pos atype'

    end <- getSourcePos

    pure $ TApp types :- Just (init, end)

atype' :: Parser (Annotated Type)
atype' = 
    let tuple = do
            pos <- indentLevel
            lexemeN . parens $ do { t1 <- sameOrIndented pos type'
                                  ; tk <- some $ do
                                        sameOrIndented pos (string ",")
                                        sameOrIndented pos type'
                                  ; pure $ TTuple (t1 : tk) }
        list = do
            pos <- indentLevel
            lexemeN . brackets $ do { t1 <- sameOrIndented pos type'
                                        ; ts <- many $ do
                                            sameOrIndented pos (string ",")
                                            sameOrIndented pos type'
                                        ; pure $ TList (t1 : ts) }
                                 <|> (string "" $> TList [])
    in do
        init <- getSourcePos
        t <- gtycon'
            <|> TVar <$> lexemeN typeVariable
            <|> try tuple
            <|> list
            <|> getAnnotated <$> parens type'
        end <- getSourcePos

        pure $ t :- Just (init, end)


gtycon' :: Parser Type
gtycon' = conid' <|> (try (string "()") $> TTuple [])

conid' :: Parser Type
conid' = TId <$> typeIdentifier