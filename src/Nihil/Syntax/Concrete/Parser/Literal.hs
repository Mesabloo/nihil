{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Nihil.Syntax.Concrete.Parser.Literal
( pFloat, pInteger, pCharacter, pString ) where

import Nihil.Syntax.Common (Parser)
import Nihil.Utils.Source
import Nihil.Syntax.Concrete.Parser
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MPC
import qualified Text.Megaparsec.Char.Lexer as MPL
import qualified Data.Text as Text
import Control.Applicative ((<|>))

{-| A lexer for integral numbers. It supports binary, hexadecimal and even octal formatting.

    An integer has the following BNF:

    @\<integer\>     ::= \<hexadecimal\> | \<binary\> | \<octal\> | \<decimal\> ;@
    @\<hexadecimal\> ::= (\'0x\' | \'0X\') [ \<hexDigit\> ] ;@
    @\<binary\>      ::= (\'0b\' | \'0B\') [ \<bindigit\> ] ;@
    @\<octal\>       ::= (\'0o\' | \'0O\') [ \<octalDigit\> ] ;@
    @\<decimal\>     ::= [ \<decimalDigit\> ] ;@
-}
pInteger :: Parser (Located Integer)
pInteger = lexeme do
    withPosition integer
  where integer = hex <|> bin <|> oct <|> int
        hex     = (MPC.string "0x" <|> MPC.string "0X") *> MPL.hexadecimal
        bin     = (MPC.string "0b" <|> MPC.string "0B") *> MPL.binary
        oct     = (MPC.string "0o" <|> MPC.string "0O") *> MPL.octal
        int     = MPL.decimal

{-| A lexer for floating point numbers. It supports exponent notation.

    A floating point number has the following BNF:

    @\<float\> ::= \<integer\> \'.\' \<integer\> { (\'e\' | \'E\') \<exponent\> } ;@
-}
pFloat :: Parser (Located Double)
pFloat = lexeme do
    withPosition MPL.float

{-| A parser for a char literal. It takes in account escape characters such as @\n@ or @\e@.

    A character literal has the following BNF:

    @\<character\> ::= '\'' (\<escapeCharacter\> | \<anyCharacter\>) '\'' ;@
-}
pCharacter :: Parser (Located Char)
pCharacter = lexeme do
    withPosition (MPC.char '\'' *> anyChar <* MPC.char '\'')

anyChar :: Parser Char
anyChar = MPL.charLiteral

{-| A parser for a string literal. It takes in account escape characters just like 'pChar'.

    A string literal has the following BNF:

    @\<string\> ::= '"' [\<escapeCharacter\> | \<anyCharacter\>] '"' ;@
-}
pString :: Parser (Located Text.Text)
pString = lexeme do
    withPosition (Text.pack <$> (MPC.char '"' *> MP.manyTill anyChar (MPC.char '"')))