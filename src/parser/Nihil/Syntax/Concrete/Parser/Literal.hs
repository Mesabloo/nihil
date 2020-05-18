{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Nihil.Syntax.Concrete.Parser.Literal
( pFloat, pInteger, pCharacter, pString ) where

import Nihil.Syntax.Common (Parser)
import Nihil.Utils.Source
import Nihil.Utils.Impossible
import Nihil.Syntax.Concrete.Parser
import Nihil.Syntax.Concrete.Core (Literal(..))
import Nihil.Syntax.Concrete.Lexer
import qualified Text.Megaparsec as MP

{-| A lexer for integral numbers. It supports binary, hexadecimal and even octal formatting.

    An integer has the following BNF:

    @\<integer\>     ::= \<hexadecimal\> | \<binary\> | \<octal\> | \<decimal\> ;@
    @\<hexadecimal\> ::= (\'0x\' | \'0X\') [ \<hexDigit\> ] ;@
    @\<binary\>      ::= (\'0b\' | \'0B\') [ \<bindigit\> ] ;@
    @\<octal\>       ::= (\'0o\' | \'0O\') [ \<octalDigit\> ] ;@
    @\<decimal\>     ::= [ \<decimalDigit\> ] ;@
-}
pInteger :: Parser (Located Literal)
pInteger = lexeme do
    withPosition (LInteger . extract . annotated <$> MP.satisfy (f . annotated))
  where f (TkInt _) = True
        f _         = False

        extract (TkInt i) = i
        extract t         = impossible ("Cannot extract integer from " <> show t)

{-| A lexer for floating point numbers. It supports exponent notation.

    A floating point number has the following BNF:

    @\<float\> ::= \<integer\> \'.\' \<integer\> { (\'e\' | \'E\') \<exponent\> } ;@
-}
pFloat :: Parser (Located Literal)
pFloat = lexeme do
    withPosition (LDouble . extract . annotated <$> MP.satisfy (f . annotated))
  where f (TkFloat _) = True
        f _           = False

        extract (TkFloat f) = f
        extract t           = impossible ("Cannot extract float from " <> show t)

{-| A parser for a char literal. It takes in account escape characters such as @\n@ or @\e@.

    A character literal has the following BNF:

    @\<character\> ::= '\'' (\<escapeCharacter\> | \<anyCharacter\>) '\'' ;@
-}
pCharacter :: Parser (Located Literal)
pCharacter = lexeme do
    withPosition (LCharacter . extract . annotated <$> MP.satisfy (f . annotated))
  where f (TkChar _) = True
        f _          = False

        extract (TkChar c) = c
        extract t          = impossible ("Cannot extract character from " <> show t)

{-| A parser for a string literal. It takes in account escape characters just like 'pChar'.

    A string literal has the following BNF:

    @\<string\> ::= '"' [\<escapeCharacter\> | \<anyCharacter\>] '"' ;@
-}
pString :: Parser (Located Literal)
pString = lexeme do
    withPosition (LString . extract . annotated <$> MP.satisfy (f . annotated))
  where f (TkString _) = True
        f _            = False

        extract (TkString s) = s
        extract t            = impossible ("Cannot extract string from " <> show t)
