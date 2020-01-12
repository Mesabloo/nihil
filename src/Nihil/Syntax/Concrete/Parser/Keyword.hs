module Nihil.Syntax.Concrete.Parser.Keyword where

import Nihil.Syntax.Common (Parser)
import Nihil.Syntax.Concrete.Lexeme
import Nihil.Utils.Source
import Nihil.Syntax.Concrete.Debug
import qualified Text.Megaparsec as MP
import qualified Data.Text as Text

pKeyword :: String -> Parser ALexeme
pKeyword requested = debug ("pKeyword '" <> requested <> "'") (MP.satisfy (isRequestedKeyword . annotated) MP.<?> ("keyword " <> requested))
  where isRequestedKeyword (LKeyword k) = Text.unpack k == requested
        isRequestedKeyword ____________ = False