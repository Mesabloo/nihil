{-# LANGUAGE OverloadedStrings #-}

module Nihil.Syntax.Concrete.Parser.Comment
( pLineComment, pBlockComment ) where

import Nihil.Syntax.Common (Parser)
import qualified Text.Megaparsec.Char.Lexer as MPL

{-| Skips a line comment.

    A line comment has the following BNF:

    @\<lineComment\> ::= "--" [ \<anyCharacterButEOL\> ] ;@
-}
pLineComment :: Parser ()
pLineComment = MPL.skipLineComment "--"

{-| Skips a block comment.
    Block comments cannot be nested, i.e. trying to parse @{\- comments {\- nested -\} -\}@
    will throw a parse error.

    A block comment has the following BNF:

    @\<blockComment\> ::= "{\-" [ \<anyCharacter\> ] "-\}" ;@
-}
pBlockComment :: Parser ()
pBlockComment = MPL.skipBlockComment "{-" "-}"