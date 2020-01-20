{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}

module Nihil.Syntax.Concrete.Lexer.Keyword
( keywords, pKeyword ) where

import Nihil.Syntax.Common (Lexer)
import Nihil.Syntax.Concrete.Lexer (withPosition, lexeme)
import Nihil.Syntax.Concrete.Lexeme
import qualified Data.Text as Text
import qualified Text.Megaparsec.Char as MPC
import qualified Text.Megaparsec as MP
import qualified Data.Char as Ch (isPrint, isSpace)
import Control.Arrow ((&&&), (<<<), (>>>))

-- | A list of all the keywords available in the language.
keywords :: [Text.Text]
keywords =
    [ "match", "with"
    , "data", "type"
    , "let", "in", "where"
    , "infixl", "infixr" ]

-- | A lexer for any keyword (found in 'keywords').
pKeyword :: Lexer Token
pKeyword = lexeme do
    kw <- withPosition (LKeyword <$> (MP.choice (MP.try . (<* MP.notFollowedBy anyPrintableChar) . MPC.string <$> keywords)))
    pure (Token (Just kw))
  where anyPrintableChar :: Lexer Char
        anyPrintableChar = MP.satisfy (Ch.isPrint &&& (not <<< Ch.isSpace) >>> uncurry (&&))