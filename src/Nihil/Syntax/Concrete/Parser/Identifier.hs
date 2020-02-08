{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BlockArguments #-}

module Nihil.Syntax.Concrete.Parser.Identifier
( pIdentifier, pIdentifier', pSymbol, pSymbol', pAnySymbolᵉ, pAnySymbolᵗ, pUnderscore ) where

import Nihil.Syntax.Common (Parser)
import Nihil.Utils.Source
import Nihil.Utils.Annotation (hoistAnnotated)
import Nihil.Syntax.Concrete.Debug
import Nihil.Syntax.Concrete.Parser.Keyword (keywords)
import Nihil.Syntax.Concrete.Parser
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MPC
import qualified Data.Text as Text
import Control.Applicative ((<|>))
import Control.Monad (void)
import Data.Bifunctor (first)

{-| A parser for identifiers beginning with a lowercased letter.

    A lowercased identifier as the following BNF:

    @\<lowIdentifier\> ::= \<lowerChar\> [ \<alphaNumChar\> | '\'' | '_' ] ;@
-}
pIdentifier :: Parser (Located Text.Text)
pIdentifier = debug "pIdentifier" $ lexeme do
    withPosition (identifier MPC.lowerChar >>= check)
  where check x | x `elem` keywords = fail ("“" <> Text.unpack x <> "” is a keyword and thus cannot be used as an identifier")
                | otherwise         = pure x

{-| A parser for identifiers beginning with a uppercased letter.

    A uppercased identifier has the following BNF:

    @\<upIdentifier\> ::= \<upperChar\> [ \<alphaNumChar\> | '\'' | '_' ] ;@
-}
pIdentifier' :: Parser (Located Text.Text)
pIdentifier' = debug "pIdentifier'" $ lexeme do
    withPosition (identifier MPC.upperChar)

identifier :: Parser Char -> Parser Text.Text
identifier front =
    Text.pack <$> ((:) <$> front <*> MP.many (MPC.alphaNumChar <|> MPC.char '\'' <|> MPC.char '_'))

-- | A parser for symbols. A symbol is any sequence of ASCII or Unicode non-letter characters.
pSymbol :: Parser (Located Text.Text)
pSymbol = debug "pSymbol" $ lexeme do
    withPosition (Text.pack <$> (unarySymbol <|> multiSymbol))

-- | Tries to parse a given symbol.
pSymbol' :: Text.Text -> Parser ()
pSymbol' s = debug "pSymbol'" $ lexeme do
    void (MPC.string s)

unarySymbol :: Parser String
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

multiSymbol :: Parser String
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

{-| A parser for type holes or anonymous patterns.

    An underscore might be multiple underscores right next to each other.
    For example, @_____@ is parsed as a single 'LUnderscore', but @__ _@ is parsed as two 'LUnderscore's.
-}
pUnderscore :: Parser (Located ())
pUnderscore = lexeme do
    withPosition (void (MP.some (MPC.char '_')))

pAnySymbolᵉ :: Parser (Located String)
pAnySymbolᵉ = debug "pAnySymbolᵉ" $ pSymbol >>= check
  where check l@(annotated -> s)
            | s `elem` reservedExpressionOperators = fail "Cannot use reserved operator as an operator"
            | otherwise                            = pure (hoistAnnotated (first Text.unpack) l)

pAnySymbolᵗ :: Parser (Located String)
pAnySymbolᵗ = debug "pAnySymbolᵗ" $ pSymbol >>= check
  where check l@(annotated -> s)
            | s `elem` reservedTypeOperators = fail "Cannot use reserved operator as an operator"
            | otherwise                      = pure (hoistAnnotated (first Text.unpack) l)

reservedExpressionOperators :: [Text.Text]
reservedExpressionOperators = [ "=", ":", "\\", "λ", "->", ",", "→", "`", "|", "(", ")", "{", ";", "}" ]

reservedTypeOperators :: [Text.Text]
reservedTypeOperators = [ ":", "=>", "⇒", "|", ",", "(", ")", ";" ]