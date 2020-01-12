{-# LANGUAGE BlockArguments #-}

module Nihil.Syntax.Concrete.Parser
( -- * Getting source position
  getSourcePos, withPosition
  -- * Adjusting indentation levels
, sameLineOrIndented, sameIndentLevel, sameColumn, sameLineOrColumn, nonIndented ) where

import Nihil.Syntax.Common (Parser)
import Nihil.Utils.Source
import Nihil.Utils.Debug
import Data.Functor ((<&>))
import Control.Lens ((^.))
import qualified Text.Megaparsec as MP
import Control.Monad (unless, guard)
import Control.Applicative ((<|>))

-- | Gets the source position of the next token, unwrapping it from the 'Nihil.Utils.Source.Located' data type.
getSourcePos :: Parser SourcePos
getSourcePos = MP.try (MP.lookAhead MP.anySingle) <&> location

-- | Checks if a parser is eligible for being runned.
--
--   The parser will be run only if the position retrieved via 'getSourcePos' is either
--
--   - on the same line than the position passed
--
--   - on a further line, but more indented than the position passed.
--
--   If both cases fail, this parser alos 'fail's.
sameLineOrIndented :: SourcePos -> Parser a -> Parser a
sameLineOrIndented pos parser = do
    pos2 <- getSourcePos

    warnWhen (pos == NoSource || pos2 == NoSource) "Unsourced tokens may be hard to keep track of!" (pure ())

    unless (pos ^. sourceLine == pos2 ^. sourceLine) do
        let indent = pos ^. indentLevel
        guard (pos2 ^. indentLevel > indent)
            <|> fail ("Possible incorrect indentation (should be greater than " <> show indent <> ")")
    parser

-- | Checks if a parser is eligible for being runned.
--
--   The parser will be run only if the position of the next token is on the same indent level as
--   the position passed.
--
--   If not, this parser 'fail's.
sameIndentLevel :: SourcePos -> Parser a -> Parser a
sameIndentLevel pos parser = do
    pos2 <- getSourcePos

    warnWhen (pos == NoSource || pos2 == NoSource) "Unsourced tokens may be hard to keep track of!" (pure ())

    let indent = pos ^. indentLevel
    guard (pos2 ^. indentLevel == indent)
        <|> fail ("Possible incorrect indentation (should equal " <> show indent <> ")")
    parser

-- | This parser is similar to 'sameIndentLevel' but it checks for column alignment instead of indentation level.
sameColumn :: SourcePos -> Parser a -> Parser a
sameColumn pos parser = do
    pos2 <- getSourcePos

    warnWhen (pos == NoSource || pos2 == NoSource) "Unsourced tokens may be hard to keep track of!" (pure ())

    let column = pos ^. sourceColumn
    guard (pos2 ^. sourceColumn == column)
        <|> fail ("Possible incorrect alignment (should be on column " <> show column <> ")")
    parser

-- | This parser is analogous to 'sameLineOrIndented', except that it checks for the column alignment
--   instead of indentation levels.
sameLineOrColumn :: SourcePos -> Parser a -> Parser a
sameLineOrColumn pos parser = do
    pos2 <- getSourcePos

    warnWhen (pos == NoSource || pos2 == NoSource) "Unsourced tokens may be hard to keep track of!" (pure ())

    unless (pos ^. sourceLine == pos2 ^. sourceLine) do
        let column = pos ^. sourceColumn
        guard (pos2 ^. sourceColumn == column)
            <|> fail ("Possible incorrect alignment (should be on column " <> show column <> ")")
    parser

-- | Checks if a parser is eligible for being runned.
--
--   The parser will be run only if the position retrieved has an indentation level of 0.
--
--   You may define this parser as
--
--   @'nonIndented' parser = do
--    pos <- 'getSourcePos'
--    'sameIndentLevel' (pos . 'indentLevel' 'Control.Lens..~' 0) parser @
nonIndented :: Parser a -> Parser a
nonIndented parser = do
    pos2 <- getSourcePos

    warnWhen (pos2 == NoSource) "Unsourced tokens may be hard to keep track of!" (pure ())

    guard (pos2 ^. indentLevel == 0)
        <|> fail "Possible incorrect indentation (should equal 0)"
    parser

-- | Runs a parser and annotates its result with its beginning parsing position.
withPosition :: Parser a -> Parser (Located a)
withPosition parse = do
    pos <- getSourcePos
    a <- parse
    pure (locate a pos)