{-# LANGUAGE LambdaCase, OverloadedStrings #-}

module Blob.Parsing.Parser where

import Blob.Parsing.Types
import Blob.Parsing.Lexer
import Text.Megaparsec as Mega hiding (eof, match)
import Text.Megaparsec.Char hiding (string)
import qualified Data.Map as Map
import Data.Functor
import Text.Megaparsec.Char.Lexer hiding (lexeme, nonIndented, symbol, float)
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

----------------------------------------------------------------------------------------------------------
{- Types parsing -}

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

-----------------------------------------------------------------------------------------------------------------------------
{- Exprs parsing -}

parseExpression :: Parser (Annotated Expr)
parseExpression = do
    init <- getSourcePos

    pos <- indentLevel
    a <- lexemeN $ some (sameOrIndented pos atom)
    end' <- getSourcePos
    t <- optional $ (hidden (string "::") <|> string "∷") *> type'

    end <- getSourcePos

    case t of
        Nothing -> pure $ a :- Just (init, end)
        Just ty -> pure $ [AAnn (a :- Just (init, end')) ty :- Just (init, end)] :- Just (init, end)

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
        <|> AId <$> (identifier <|> try (parens opSymbol <?> "operator") <|> (try (parens (string "")) $> "()") <|> typeIdentifier)
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
list = brackets $ do { e1 <- parseExpression
                     ; es <- many (string "," *> parseExpression)
                     ; pure $ AList (e1 : es) }
                  <|> (string "" $> AList [])

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
pattern' = do
    init <- getSourcePos
    pats <- lexemeN $ some (patTerm <|> try patOperator)
    end <- getSourcePos

    pure pats

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
        <|> PCtor       <$> typeIdentifier <*> (fmap (: []) <$> many patTerm)
        <|>                 try patternTuple
        <|>                 patternList
        <|> PLit . LStr <$> string''
        <|> PParens     <$> parens pattern'
    end' <- getSourcePos
    t <- optional $ (hidden (string "::") <|> string "∷") *> type'
    end <- getSourcePos

    case t of
        Nothing -> pure (p :- Just (init, end))
        Just ty -> pure (PAnn [p :- Just (init, end')] ty :- Just (init, end))
  where
    patternList :: Parser Pattern
    patternList =   brackets (do
                                    e1 <- pattern'
                                    es <- many (string "," *> pattern')

                                    pure $ PList (e1 : es)
                            <|> string "" $> PList [])

    patternTuple :: Parser Pattern
    patternTuple = parens $ do
        e1 <- pattern'
        es <- some (string "," *> pattern')
        pure $ PTuple (e1:es)