-- Blobc, a compiler for compiling Blob source code
-- Copyright (c) 2019 Mesabloo

-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.

-- You should have received a copy of the GNU General Public License
-- along with this program. If not, see <http://www.gnu.org/licenses/>.

module Blob.Language.Syntax.Internal.Parsing.Helpers where

import Blob.Language.Syntax.Parser (Parser)
import Blob.Language.Syntax.Internal.Lexing.SourceSpan (SourceSpan(..))
import Blob.Language.Syntax.Tokens.Token (Token(..))
import Blob.Language.Syntax.Rules.Parsing.Symbol (symbol)
import Text.Megaparsec (stateInput, getParserState, try, sourceLine, sourceColumn, lookAhead, anySingle, eof)
import Control.Applicative ((<|>))
import Control.Monad (guard)
import Data.Functor (($>))

-- | A function for getting the position in the source, as well as the current indentation, for a specific lexeme.
getPositionInSource :: Parser a -> Parser (SourceSpan, a)
getPositionInSource p = do
    (_, SourceSpan pInit _) <- getPositionAndIndent
    stream <- stateInput <$> getParserState
    res <- p
    let f (Token i pos _) = (i, pos)
    (_, SourceSpan _ pEnd) <- (eof $> f (last stream)) <|> getPositionAndIndent

    pure (SourceSpan pInit pEnd, res)

-- | A function which checks whether the next token is on the same line, or more indented than the last one.
sameLineOrIndented :: (Int, SourceSpan) -> Parser a -> Parser a
sameLineOrIndented (indent, SourceSpan beg _) p = do
    (i, SourceSpan b _) <- try getPositionAndIndent
    if sourceLine beg == sourceLine b
    then p
    else do
        guard (i > indent)
            <|> fail ("Possible incorrect indentation (should be greater than " <> show indent <> ")")
        p

-- | A function which checks whether two tokens are on the same indentation level.
sameIndented :: (Int, SourceSpan) -> Parser a -> Parser a
sameIndented (indent, _) p = do
    (i, _) <- try getPositionAndIndent
    guard (i == indent)
        <|> fail ("Possible incorrect indentation (should equal " <> show indent <> ")")
    p

-- | A function checking whether two tokens are aligned (same column) or not.
aligned :: (Int, SourceSpan) -> Parser a -> Parser a
aligned (_, SourceSpan b1 _) p = do
    (_, SourceSpan b2 _) <- try getPositionAndIndent
    guard (sourceColumn b1 == sourceColumn b2)
        <|> fail ("Possible incorrect alignment (should be on column " <> show (sourceColumn b2) <> ")")
    p

-- | A function which checks whether two tokens are on the same line, or aligned on multiple lines.
sameIndentedOrLine :: (Int, SourceSpan) -> Parser a -> Parser a
sameIndentedOrLine (indent, SourceSpan beg _) p = do
    (i, SourceSpan b _) <- try getPositionAndIndent
    if sourceLine beg == sourceLine b
    then p
    else do
        guard (i == indent)
            <|> fail ("Possible incorrect indentation (should equal " <> show indent <> ")")
        p

-- | A function which checks if a token is not indented.
nonIndented :: Parser a -> Parser a
nonIndented p = do
    (i, _) <- try getPositionAndIndent
    guard (i == 0)
        <|> fail "Possible incorrect indentation (should equal 0)"
    p

-- | A simple function for returning the current indentation level, as well as the position.
getPositionAndIndent :: Parser (Int, SourceSpan)
getPositionAndIndent =
    try (lookAhead anySingle)
        >>= \(Token i p _) -> pure (i, p)

parens :: Parser a -> Parser a
parens p = symbol "(" *> p <* symbol ")"

brackets :: Parser a -> Parser a
brackets p = symbol "[" *> p <* symbol "]"

ticks :: Parser a -> Parser a
ticks p = symbol "`" *> p <* symbol "`"
