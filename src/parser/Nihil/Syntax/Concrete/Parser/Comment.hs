{-# LANGUAGE OverloadedStrings #-}

module Nihil.Syntax.Concrete.Parser.Comment
( pLineComment, pBlockComment ) where

import Nihil.Syntax.Common (Parser)
import Nihil.Utils.Source
import Nihil.Syntax.Concrete.Lexer
import qualified Text.Megaparsec as MP

{-| Skips a line comment.

    A line comment has the following BNF:

    @\<lineComment\> ::= "--" [ \<anyCharacterButEOL\> ] ;@
-}
pLineComment :: Parser ()
pLineComment = () <$ MP.satisfy (f . annotated)
  where f (TkInlineComment _) = True
        f _                   = False

{-| Skips a block comment.
    Block comments cannot be nested, i.e. trying to parse @{\- comments {\- nested -\} -\}@
    will throw a parse error.

    A block comment has the following BNF:

    @\<blockComment\> ::= "{\-" [ \<anyCharacter\> ] "-\}" ;@
-}
pBlockComment :: Parser ()
pBlockComment = () <$ MP.satisfy (f . annotated)
  where f (TkMultilineComment _) = True
        f _                      = False
