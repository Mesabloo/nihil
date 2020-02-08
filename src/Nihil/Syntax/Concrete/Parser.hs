{-# LANGUAGE BlockArguments #-}

module Nihil.Syntax.Concrete.Parser
( -- * Getting source position
  getSourcePos, withPosition
  -- * Indentation-sensitive parsing
, nonIndented, indentBlock, lineFold, lexeme, spacen1, MPL.IndentOpt(..) ) where

import Nihil.Syntax.Common (Parser)
import Nihil.Utils.Source
import Nihil.Syntax.Concrete.Parser.Comment
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MPC
import qualified Text.Megaparsec.Char.Lexer as MPL
import Control.Monad (void)
import qualified Data.Char as Ch

-- | Gets the source position of the next token, unwrapping it from the 'Nihil.Utils.Source.Located' data type.
getSourcePos :: Parser SourcePos
getSourcePos = fromSourcePos <$> MP.getSourcePos

-- | Runs a parser and annotates its result with its beginning parsing position.
withPosition :: Parser a -> Parser (Located a)
withPosition parse = do
    pos <- getSourcePos
    a <- parse
    pure (locate a pos)

isSpace :: Char -> Bool
isSpace c =
    let code = Ch.ord c
    in code == 9 || code == 32 || code == 160 || code == 8200 || code == 8201 || code == 8202

space1 :: Parser ()
space1 = MPL.space sc1 pLineComment pBlockComment

spacen1 :: Parser ()
spacen1 = MPL.space scn1 pLineComment pBlockComment

sc1 :: Parser ()
sc1 = void (MP.satisfy isSpace)

scn1 :: Parser ()
scn1 = void MPC.spaceChar

-- | See @'MPL.nonIndented'@.
nonIndented :: Parser a -> Parser a
nonIndented = MPL.nonIndented spacen1

-- | See @'MPL.indentBlock'@.
indentBlock :: Parser (MPL.IndentOpt Parser a b) -> Parser a
indentBlock = MPL.indentBlock spacen1

-- | See @'MPL.lineFold'@.
lineFold :: (Parser () -> Parser a) -> Parser a
lineFold = MPL.lineFold spacen1

-- | See @'MPL.lexeme'@.
lexeme :: Parser a -> Parser a
lexeme = MPL.lexeme space1