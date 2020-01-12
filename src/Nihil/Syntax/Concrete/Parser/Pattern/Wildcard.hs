module Nihil.Syntax.Concrete.Parser.Pattern.Wildcard where

import Nihil.Syntax.Common (Parser)
import Nihil.Syntax.Concrete.Core
import Nihil.Syntax.Concrete.Lexeme
import Nihil.Utils.Source
import Nihil.Syntax.Concrete.Debug
import qualified Text.Megaparsec as MP

pWildcard :: Parser Pattern
pWildcard = debug "pWildcard" (PWildcard <$ MP.satisfy (isWildcard . annotated) MP.<?> "wildcard")
  where isWildcard LUnderscore = True
        isWildcard ___________ = False