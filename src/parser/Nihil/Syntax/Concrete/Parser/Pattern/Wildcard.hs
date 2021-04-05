module Nihil.Syntax.Concrete.Parser.Pattern.Wildcard where

import Nihil.Syntax.Common (Parser)
import Nihil.Syntax.Concrete.Core
import Nihil.Syntax.Concrete.Parser.Identifier (pUnderscore)
import Nihil.Syntax.Concrete.Debug
import qualified Text.Megaparsec as MP

pWildcard :: Parser Pattern
pWildcard = debug "pWildcard" (PWildcard <$ pUnderscore MP.<?> "wildcard")