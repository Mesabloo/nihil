{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Nihil.Syntax.Concrete.Lexer.Number
( pInteger, pFloat ) where

import Nihil.Syntax.Common (Lexer)
import Nihil.Syntax.Concrete.Lexeme
import Nihil.Syntax.Concrete.Lexer (lexeme, withPosition)
import qualified Text.Megaparsec.Char.Lexer as MPL
import qualified Text.Megaparsec.Char as MPC
import Control.Applicative ((<|>))

{-| A lexer for integral numbers. It supports binary, hexadecimal and even octal formatting.

    An integer has the following BNF:

    @\<integer\>     ::= \<hexadecimal\> | \<binary\> | \<octal\> | \<decimal\> ;@
    @\<hexadecimal\> ::= (\'0x\' | \'0X\') [ \<hexDigit\> ] ;@
    @\<binary\>      ::= (\'0b\' | \'0B\') [ \<bindigit\> ] ;@
    @\<octal\>       ::= (\'0o\' | \'0O\') [ \<octalDigit\> ] ;@
    @\<decimal\>     ::= [ \<decimalDigit\> ] ;@
-}
pInteger :: Lexer Token
pInteger = lexeme do
    i <- withPosition (LInteger <$> integer)
    pure (Token (Just i))
  where integer = hex <|> bin <|> oct <|> int
        hex     = (MPC.string "0x" <|> MPC.string "0X") *> MPL.hexadecimal
        bin     = (MPC.string "0b" <|> MPC.string "0B") *> MPL.binary
        oct     = (MPC.string "0o" <|> MPC.string "0O") *> MPL.octal
        int     = MPL.decimal

{-| A lexer for floating point numbers. It supports exponent notation.

    A floating point number has the following BNF:

    @\<float\> ::= \<integer\> \'.\' \<integer\> { (\'e\' | \'E\') \<exponent\> } ;@
-}
pFloat :: Lexer Token
pFloat = lexeme do
    d <- withPosition (LFloat <$> MPL.float)
    pure (Token (Just d))