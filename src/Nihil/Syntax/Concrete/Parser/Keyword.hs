{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Nihil.Syntax.Concrete.Parser.Keyword where

import Nihil.Syntax.Common (Parser)
import Nihil.Syntax.Concrete.Parser
import Nihil.Syntax.Concrete.Lexer
import Nihil.Utils.Source
import Nihil.Syntax.Concrete.Debug
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MPC
import qualified Data.Text as Text
import qualified Data.Char as Ch
import Control.Monad (guard, void)
import Control.Applicative ((<|>))

-- | A lexer for any keyword (found in 'keywords').
pKeyword :: Text.Text -> Parser ()
pKeyword kw = debug "pKeyword" $ lexeme do
    () <$ MP.satisfy (f kw . annotated)
  where f "match"  TkMatch  = True
        f "with"   TkWith   = True
        f "data"   TkData   = True
        f "type"   TkType   = True
        f "let"    TkLet    = True
        f "in"     TkIn     = True
        f "where"  TkWhere  = True
        f "infixl" TkInfixL = True
        f "infixr" TkInfixR = True
        f _        _        = False
