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

module Blob.Language.Syntax where

import Blob.Language.Syntax.Rules.Lexing.Program (tokens)
import Blob.Language.Syntax.Lexer (initLexState)
import Blob.Language.Syntax.Tokens.Token (Token(..))
import Blob.Language.Syntax.Parser (Parser)
import Blob.Language.Syntax.Internal.Parsing.AST (Program)
import Blob.Language.Syntax.Rules.Parsing.Program (program)
import Text.Megaparsec (ParseErrorBundle, runParserT, runParser)
import Control.Monad.State (evalState)
import Data.Text (Text)
import Data.Void (Void)
import Data.Maybe (mapMaybe)

-- | A simple wrapper function for running the lexer with some text as input.
runLexer :: Text -> String -> Either (ParseErrorBundle Text Void) [Token]
runLexer content fileName =
    evalState (runParserT tokens fileName content) initLexState

-- | A simple wrapper function for running the 'program' 'Parser' on a stream of 'Token's.
runParser :: [Token] -> String -> Either (ParseErrorBundle [Token] Void) Program
runParser = runParser' program

-- | A simple wrapper function for running a 'Parser' on a stream of 'Token's.
runParser' :: Parser a -> [Token] -> String -> Either (ParseErrorBundle [Token] Void) a
runParser' p tks fileName = Text.Megaparsec.runParser p fileName (mapMaybe f tks)
  where f (Token _ _ Nothing)   = Nothing
        f (Token indent spos x) = Just (Token indent spos x)