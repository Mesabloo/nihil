module Nihil.Syntax.Concrete.Parser.Expression.TypeHole where

import Nihil.Syntax.Common (Parser)
import Nihil.Syntax.Concrete.Lexeme
import Nihil.Syntax.Concrete.Core
import Nihil.Utils.Source
import Nihil.Syntax.Concrete.Debug
import qualified Text.Megaparsec as MP

pTypeHole :: Parser Atom
pTypeHole = debug "pTypeHole" (ATypeHole <$ MP.satisfy (isTypeHole . annotated) MP.<?> "type hole")
  where isTypeHole LUnderscore = True
        isTypeHole ___________ = False