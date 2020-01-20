module Nihil.Syntax.Concrete.Lexer.Program
( lProgram ) where

import Nihil.Syntax.Common (Lexer, currentIndent)
import Nihil.Syntax.Concrete.Lexer
import Nihil.Syntax.Concrete.Lexeme
import Nihil.Syntax.Concrete.Lexer.Keyword
import Nihil.Syntax.Concrete.Lexer.Character
import Nihil.Syntax.Concrete.Lexer.Number
import Nihil.Syntax.Concrete.Lexer.Identifier
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MPC
import Control.Lens ((.=))
import Data.Functor ((<&>))

-- | The main 'Lexer' used for tokenizing a whole source file.
lProgram :: Lexer [Token]
lProgram = (lexeme (beginning *> MP.many tokens) <* MP.eof) <&> filter isReal
  where beginning = indent *> lexeme (pure ())

        isReal (Token Nothing) = False
        isReal _               = True

tokens :: Lexer Token
tokens = lexeme $ MP.choice
    [ pKeyword                                        -- Special
    , pString, pChar, MP.try pFloat, pInteger         -- Literals
    , pSymbol, pIdentifier, pIdentifier', pUnderscore -- Identifiers
    , pEOL ]                                          -- Special

{-| A lexer for end of lines. The main existance of this lexer is to get the
    indentation level after each end of line, to keep track of them (using 'indent').
-}
pEOL :: Lexer Token
pEOL = do
    MPC.eol
    indent
    pure (Token Nothing)

-- | The main purpose of this lexer is to register indentation levels throughout the source.
indent :: Lexer ()
indent = do
    lexeme (pure ())
    pos <- MP.getSourcePos
    currentIndent .= MP.unPos (MP.sourceColumn pos) - 1