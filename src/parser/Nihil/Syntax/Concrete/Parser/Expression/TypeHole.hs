module Nihil.Syntax.Concrete.Parser.Expression.TypeHole where

import Nihil.Syntax.Common (Parser)
import Nihil.Syntax.Concrete.Parser.Identifier (pUnderscore)
import Nihil.Syntax.Concrete.Core
import Nihil.Syntax.Concrete.Debug
import qualified Text.Megaparsec as MP

pTypeHole :: Parser Atom
pTypeHole = debug "pTypeHole" (ATypeHole <$ pUnderscore MP.<?> "type hole")