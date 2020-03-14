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
import Control.Monad (guard, void)
import Control.Applicative ((<|>))

-- | A list of all the keywords available in the language.
keywords :: [Text.Text]
keywords =
    [ "match", "with"
    , "data", "type", "record"
    , "let", "in", "where"
    , "infixl", "infixr"
    , "Π" ]

-- | A lexer for any keyword (found in 'keywords').
pKeyword :: Text.Text -> Parser ()
pKeyword kw = debug "pKeyword" $ lexeme do
    guard (kw `elem` keywords)
        <|> fail "Trying to parse non-keyword in keyword parser"
    void (MP.try (MPC.string kw <* MP.notFollowedBy anyLetterChar))
  where anyLetterChar :: Parser Char
        anyLetterChar = MP.satisfy (Ch.isLetter)
