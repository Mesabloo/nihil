{-# LANGUAGE BlockArguments #-}

module Nihil.Syntax.Concrete.Lexer.Identifier
( pIdentifier, pIdentifier', pSymbol, pUnderscore ) where

import Nihil.Syntax.Common (Lexer)
import Nihil.Syntax.Concrete.Lexer (withPosition, lexeme)
import Nihil.Syntax.Concrete.Lexeme
import Nihil.Syntax.Concrete.Lexer.Keyword (keywords)
import qualified Text.Megaparsec.Char as MPC
import qualified Text.Megaparsec as MP
import qualified Data.Text as Text
import Control.Applicative ((<|>))

{-| A lexer for identifiers beginning with a lowercased letter.

    A lowercased identifier as the following BNF:

    @\<lowIdentifier\> ::= \<lowerChar\> [ \<alphaNumChar\> | '\'' | '_' ] ;@
-}
pIdentifier :: Lexer Token
pIdentifier = lexeme do
    i <- withPosition (LLowerIdentifier <$> (identifier MPC.lowerChar >>= check))
    pure (Token (Just i))
  where check x | x `elem` keywords = fail ("“" <> Text.unpack x <> "” is a keyword and thus cannot be used as an identifier")
                | otherwise         = pure x

{-| A lexer for identifiers beginning with a uppercased letter.

    A uppercased identifier has the following BNF:

    @\<upIdentifier\> ::= \<upperChar\> [ \<alphaNumChar\> | '\'' | '_' ] ;@
-}
pIdentifier' :: Lexer Token
pIdentifier' = lexeme do
    i <- withPosition (LUpperIdentifier <$> identifier MPC.upperChar)
    pure (Token (Just i))

identifier :: Lexer Char -> Lexer Text.Text
identifier front =
    Text.pack <$> ((:) <$> front <*> MP.many (MPC.alphaNumChar <|> MPC.char '\'' <|> MPC.char '_'))

-- | A lexer for symbols. A symbol is any sequence of ASCII or Unicode non-letter characters.
pSymbol :: Lexer Token
pSymbol = lexeme do
    s <- withPosition (LSymbol . Text.pack <$> (unarySymbol <|> multiSymbol))
    pure (Token (Just s))

unarySymbol :: Lexer String
unarySymbol =
    (:[]) <$> MP.satisfy isUnarySymbol
  where isUnarySymbol '('  = True
        isUnarySymbol ')'  = True
        isUnarySymbol '{'  = True
        isUnarySymbol '}'  = True
        isUnarySymbol ','  = True
        isUnarySymbol ';'  = True
        isUnarySymbol '\\' = True
        isUnarySymbol 'λ'  = True
        isUnarySymbol '→'  = True
        isUnarySymbol '⇒'  = True
        isUnarySymbol ':'  = True
        isUnarySymbol '`'  = True
        isUnarySymbol  _   = False

multiSymbol :: Lexer String
multiSymbol =
    MP.some (MPC.symbolChar <|> MP.satisfy isMultiSymbol)
  where isMultiSymbol '!' = True
        isMultiSymbol '$' = True
        isMultiSymbol '%' = True
        isMultiSymbol '&' = True
        isMultiSymbol '.' = True
        isMultiSymbol '<' = True
        isMultiSymbol '=' = True
        isMultiSymbol '>' = True
        isMultiSymbol '?' = True
        isMultiSymbol '^' = True
        isMultiSymbol '~' = True
        isMultiSymbol '|' = True
        isMultiSymbol '@' = True
        isMultiSymbol '*' = True
        isMultiSymbol '/' = True
        isMultiSymbol '-' = True
        isMultiSymbol '+' = True
        isMultiSymbol ':' = True
        isMultiSymbol  _  = False

{-| A lexer for type holes or anonymous patterns.

    An underscore might be multiple underscores right next to each other.
    For example, @_____@ is parsed as a single 'LUnderscore', but @__ _@ is parsed as two 'LUnderscore's.
-}
pUnderscore :: Lexer Token
pUnderscore = lexeme do
    w <- withPosition (LUnderscore <$ MP.some (MPC.char '_'))
    pure (Token (Just w))