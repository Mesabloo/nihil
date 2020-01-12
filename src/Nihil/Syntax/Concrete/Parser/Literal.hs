{-# LANGUAGE LambdaCase #-}

module Nihil.Syntax.Concrete.Parser.Literal
( pFloat, pInteger, pCharacter, pString ) where

import Nihil.Syntax.Common (Parser)
import Nihil.Syntax.Concrete.Core
import Nihil.Utils.Source
import qualified Nihil.Syntax.Concrete.Lexeme as Lex
import Nihil.Utils.Impossible (impossible)
import Nihil.Syntax.Concrete.Debug
import qualified Text.Megaparsec as MP
import Control.Arrow ((>>^))
import qualified Data.Text as Text

pFloat :: Parser Literal
pFloat = debug "pFloat" (MP.satisfy (isFloat . annotated) >>= getLiteral MP.<?> "floating point number")
  where isFloat (Lex.LFloat _) = True
        isFloat ______________ = False

pInteger :: Parser Literal
pInteger = debug "pInteger" (MP.satisfy (isInteger . annotated) >>= getLiteral MP.<?> "integer")
  where isInteger (Lex.LInteger _) = True
        isInteger ________________ = False

pCharacter :: Parser Literal
pCharacter = debug "pCharacter" (MP.satisfy (isCharacter . annotated) >>= getLiteral MP.<?> "character")
  where isCharacter (Lex.LChar _) = True
        isCharacter _____________ = False

pString :: Parser Literal
pString = debug "pString" (MP.satisfy (isString . annotated) >>= getLiteral MP.<?> "string")
  where isString (Lex.LString _) = True
        isString _______________ = False

getLiteral :: Lex.ALexeme -> Parser Literal
getLiteral = annotated >>^ \case
    Lex.LFloat f   -> pure (LDouble f)
    Lex.LInteger i -> pure (LInteger i)
    Lex.LChar c    -> pure (LCharacter c)
    Lex.LString s  -> pure (LString (Text.unpack s))
    ______________ -> impossible "cannot get any other token than a literal"