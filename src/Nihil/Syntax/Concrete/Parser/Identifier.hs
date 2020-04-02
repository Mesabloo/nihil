{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BlockArguments #-}

module Nihil.Syntax.Concrete.Parser.Identifier
( pIdentifier, pIdentifier', pSymbol, pSymbol', pAnySymbolᵉ, pAnySymbolᵗ, pUnderscore ) where

import Nihil.Syntax.Common (Parser)
import Nihil.Utils.Source
import Nihil.Utils.Impossible
import Nihil.Syntax.Concrete.Debug
import Nihil.Syntax.Concrete.Parser
import Nihil.Syntax.Concrete.Lexer
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MPC
import qualified Data.Text as Text
import Control.Applicative ((<|>))
import Control.Monad (void)

{-| A parser for identifiers beginning with a lowercased letter.

    A lowercased identifier as the following BNF:

    @\<lowIdentifier\> ::= \<lowerChar\> [ \<alphaNumChar\> | '\'' | '_' ] ;@
-}
pIdentifier :: Parser (Located String)
pIdentifier = debug "pIdentifier" $ lexeme do
    withPosition (extract . annotated <$> MP.satisfy (f . annotated))
  where f (TkLIdent _) = True
        f _            = False

        extract (TkLIdent i) = i
        extract t            = impossible ("Cannot extract identifier from " <> show t)

{-| A parser for identifiers beginning with a uppercased letter.

    A uppercased identifier has the following BNF:

    @\<upIdentifier\> ::= \<upperChar\> [ \<alphaNumChar\> | '\'' | '_' ] ;@
-}
pIdentifier' :: Parser (Located String)
pIdentifier' = debug "pIdentifier'" $ lexeme do
    withPosition (extract . annotated <$> MP.satisfy (f . annotated))
  where f (TkUIdent _) = True
        f _            = False

        extract (TkUIdent i) = i
        extract t            = impossible ("Cannot extract identifier from " <> show t)

-- | A parser for symbols. A symbol is any sequence of ASCII or Unicode non-letter characters.
pSymbol :: Parser (Located String)
pSymbol = debug "pSymbol" $ lexeme do
    withPosition (extract . annotated <$> MP.satisfy (f . annotated))
  where f (TkSym _) = True
        f _         = False

        extract (TkSym s) = s
        extract t         = impossible ("Cannot extract symbol from " <> show t)

-- | Tries to parse a given symbol.
pSymbol' :: Text.Text -> Parser ()
pSymbol' s = debug "pSymbol'" $ lexeme do
    void (MP.satisfy (f s . annotated))
  where f s (TkSym sy) = Text.pack sy == s
        f s _          = False
        f s t          = impossible ("Cannot extract symbol " <> Text.unpack s <> " from " <> show t)

{-| A parser for type holes or anonymous patterns.

    An underscore might be multiple underscores right next to each other.
    For example, @_____@ is parsed as a single 'LUnderscore', but @__ _@ is parsed as two 'LUnderscore's.
-}
pUnderscore :: Parser (Located ())
pUnderscore = lexeme do
    withPosition (() <$ MP.satisfy (f . annotated))
  where f = (== TkUnderscore)

pAnySymbolᵉ :: Parser (Located String)
pAnySymbolᵉ = debug "pAnySymbolᵉ" $ MP.try (pSymbol >>= check)
  where check l@(annotated -> s)
            | Text.pack s `elem` reservedExpressionOperators = fail "Cannot use reserved operator as an operator"
            | otherwise                                      = pure l

pAnySymbolᵗ :: Parser (Located String)
pAnySymbolᵗ = debug "pAnySymbolᵗ" $ MP.try (pSymbol >>= check)
  where check l@(annotated -> s)
            | Text.pack s `elem` reservedTypeOperators = fail "Cannot use reserved operator as an operator"
            | otherwise                                = pure l

reservedExpressionOperators :: [Text.Text]
reservedExpressionOperators = [ "=", ":", "\\", "λ", "->", ",", "→", "`", "|", "(", ")", "{", ";", "}" ]

reservedTypeOperators :: [Text.Text]
reservedTypeOperators = [ ":", "|", ",", "(", ")", ";", "{", "}", "×", "Π", "=>", "⇒" ]
