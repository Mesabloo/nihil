{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Nihil.Syntax.Concrete.Parser.Keyword where

import Nihil.Syntax.Common (Parser)
import Nihil.Syntax.Concrete.Parser
import Nihil.Syntax.Concrete.Debug
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MPC
import qualified Data.Text as Text
import qualified Data.Char as Ch
import Control.Arrow ((&&&), (<<<), (>>>))
import Control.Monad (guard, void)
import Control.Applicative ((<|>))

-- | A list of all the keywords available in the language.
keywords :: [Text.Text]
keywords =
    [ "match", "with"
    , "data", "type"
    , "class", "instance"
    , "let", "in", "where"
    , "infixl", "infixr" ]

-- | A lexer for any keyword (found in 'keywords').
pKeyword :: Text.Text -> Parser ()
pKeyword kw = debug "pKeyword" $ lexeme do
    guard (kw `elem` keywords)
        <|> fail "Trying to parse non-keyword in keyword parser"
    void (MP.try (MPC.string kw <* MP.notFollowedBy anyPrintableChar))
  where anyPrintableChar :: Parser Char
        anyPrintableChar = MP.satisfy (Ch.isPrint &&& (not <<< Ch.isSpace) >>> uncurry (&&))