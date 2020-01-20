module Nihil.Syntax.Concrete.Parser.Identifier
( pIdentifier, pIdentifier', pSymbol, pAnySymbolᵉ, pAnySymbolᵗ ) where

import Nihil.Syntax.Common (Parser)
import Nihil.Syntax.Concrete.Lexeme
import Nihil.Utils.Source
import Nihil.Utils.Impossible (impossible)
import Nihil.Syntax.Concrete.Debug
import qualified Text.Megaparsec as MP
import qualified Data.Text as Text

pIdentifier :: Parser String
pIdentifier = debug "pIdentifier" (MP.satisfy (isLowIdentifier . annotated) >>= getIdentifier MP.<?> "lowercase identifier")
  where isLowIdentifier (LLowerIdentifier _) = True
        isLowIdentifier ____________________ = False

pIdentifier' :: Parser String
pIdentifier' = debug "pIdentifier'" (MP.satisfy (isUpIdentifier . annotated) >>= getIdentifier MP.<?> "uppercase identifier")
  where isUpIdentifier (LUpperIdentifier _) = True
        isUpIdentifier ____________________ = False

getIdentifier :: ALexeme -> Parser String
getIdentifier l = case annotated l of
    LLowerIdentifier i -> pure (Text.unpack i)
    LUpperIdentifier i -> pure (Text.unpack i)
    LSymbol i          -> pure (Text.unpack i)
    __________________ -> impossible "cannot get any other token than an identifier"

pSymbol :: String -> Parser ALexeme
pSymbol requested = debug ("pSymbol '" <> requested <> "'") (MP.satisfy (isRequestedSymbol . annotated) MP.<?> ("symbol " <> requested))
  where isRequestedSymbol (LSymbol s) = requested == Text.unpack s
        isRequestedSymbol ___________ = False

pAnySymbolᵉ :: Parser String
pAnySymbolᵉ = debug "p[Expression]AnySymbol" (MP.satisfy (isNotReserved . annotated) >>= getIdentifier MP.<?> "operator")
  where isNotReserved (LSymbol s) = Text.unpack s `notElem` reservedExpressionOperators
        isNotReserved ___________ = False

pAnySymbolᵗ :: Parser String
pAnySymbolᵗ = debug "p[Type]AnySymbol" (MP.satisfy (isNotReserved . annotated) >>= getIdentifier MP.<?> "operator")
  where isNotReserved (LSymbol s) = Text.unpack s `notElem` reservedTypeOperators
        isNotReserved ___________ = False

reservedExpressionOperators :: [String]
reservedExpressionOperators = [ "=", ":", "\\", "λ", "->", ",", "→", "`", "|", "(", ")", "{", ";", "}" ]

reservedTypeOperators :: [String]
reservedTypeOperators = [ ":", "=>", "⇒", "|", ",", "(", ")", ";" ]