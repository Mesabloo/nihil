{-# LANGUAGE BlockArguments #-}

module Nihil.Syntax.Concrete.Lexer.Character
( pChar, pString ) where

import Nihil.Syntax.Common (Lexer)
import Nihil.Syntax.Concrete.Lexeme
import Nihil.Syntax.Concrete.Lexer (lexeme, withPosition)
import qualified Text.Megaparsec.Char as MPC
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char.Lexer as MPL
import qualified Data.Text as Text

{-| A lexer for a char literal. It takes in account escape characters such as @\n@ or @\e@.

    A character literal has the following BNF:

    @\<character\> ::= '\'' (\<escapeCharacter\> | \<anyCharacter\>) '\'' ;@
-}
pChar :: Lexer Token
pChar = lexeme do
    c <- withPosition (LChar <$> (MPC.char '\'' *> anyChar <* MPC.char '\''))
    pure (Token (Just c))

anyChar :: Lexer Char
anyChar = MPL.charLiteral

{-| A lexer for a string literal. It takes in account escape characters just like 'pChar'.

    A string literal has the following BNF:

    @\<string\> ::= '"' [\<escapeCharacter\> | \<anyCharacter\>] '"' ;@
-}
pString :: Lexer Token
pString = lexeme do
    s <- withPosition (LString . Text.pack <$> (MPC.char '"' *> MP.manyTill anyChar (MPC.char '"')))
    pure (Token (Just s))