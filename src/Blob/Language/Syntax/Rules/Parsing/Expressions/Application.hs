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

module Blob.Language.Syntax.Rules.Parsing.Expressions.Application where

import Blob.Language.Syntax.Parser (Parser)
import Blob.Language.Syntax.Internal.Parsing.AST
import Blob.Language.Syntax.Internal.Parsing.Helpers
import Blob.Language.Syntax.Internal.Parsing.Located
import {-# SOURCE #-} Blob.Language.Syntax.Rules.Parsing.Expressions.Atom
import Blob.Language.Syntax.Internal.Lexing.SourceSpan (begin, end, SourceSpan(..))
import Control.Applicative (some)
import Control.Lens ((^.))
import Data.Maybe (isNothing, fromJust)

app :: Parser (Located Atom)
app = do
    (_, a) <- getPositionInSource $ do
        iPos <- getPositionAndIndent
        (:) <$> exprNoApp <*> some (sameLineOrIndented iPos exprNoApp)
    pure $ foldl1 f a
  where
    f a1 a2 = let begin' = (^. begin) <$> (a1 ^. pos)
                  end'   = (^. end)   <$> (a2 ^. pos)
              in if any isNothing [ begin', end' ]
                 then AApp a1 a2 :@ Nothing
                 else AApp a1 a2 :@ Just (SourceSpan (fromJust begin') (fromJust end'))
