{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleInstances #-}

module Nihil.Syntax.Concrete.Parser
( -- * Getting source position
  getSourcePos, withPosition
  -- * Indentation-sensitive parsing
, nonIndented, indentBlock, indentLevel, lineFold
  -- * Misc
, lexeme ) where

import Nihil.Syntax.Common (Parser)
import Nihil.Utils.Source
import Nihil.Syntax.Pretty()
import qualified Nihil.Syntax.Concrete.Lexer as L
import Nihil.Syntax.Concrete.Parser.Comment
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char.Lexer as MPL
import Control.Applicative
import Prelude hiding (log)

-- | Gets the source position of the next token, unwrapping it from the 'Nihil.Utils.Source.Located' data type.
getSourcePos :: Parser SourcePos
getSourcePos = fromSourcePos <$> MP.getSourcePos

-- | Runs a parser and annotates its result with its beginning parsing position.
withPosition :: Parser a -> Parser (Located a)
withPosition parse = do
    pos <- getSourcePos
    a <- parse
    pure (locate a pos)

-- | See @'MPL.nonIndented'@.
nonIndented :: Parser a -> Parser a
nonIndented = MPL.nonIndented (MP.try space)

-- | See @'MPL.indentLevel'@.
indentLevel :: Parser MP.Pos
indentLevel = MPL.indentLevel

-- | See @'MPL.indentBlock'@.
indentBlock :: Parser a -> Parser [a]
indentBlock p = do
    MP.try space
    pos <- indentLevel
    p `MP.sepBy1` MP.try (MPL.indentGuard space EQ pos)

-- | See @'MPL.lineFold'@.
lineFold :: (Parser () -> Parser a) -> Parser a
lineFold = MPL.lineFold (MP.try space)

space :: Parser ()
space = MPL.space pEOL pLineComment pBlockComment
  where pEOL = () <$ MP.satisfy (\(annotated -> t) -> t == L.TkEOL)

lexeme :: Parser a -> Parser a
lexeme = MPL.lexeme (MP.try space)
