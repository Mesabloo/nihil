{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Nihil.Syntax.Concrete.Parser.Type
( pType ) where

import Nihil.Syntax.Common (Parser)
import Nihil.Syntax.Concrete.Core
import Nihil.Utils.Source
import Nihil.Syntax.Concrete.Parser.Type.Operator
import Nihil.Syntax.Concrete.Parser.Type.Atom
import Nihil.Syntax.Concrete.Parser.Type.Has
import Nihil.Syntax.Concrete.Parser.Identifier
import Nihil.Syntax.Concrete.Parser
import Nihil.Syntax.Concrete.Debug
import qualified Text.Megaparsec as MP
import Control.Applicative ((<|>))

pType :: Parser () -> Parser [AType]
pType s = debug "pType" $ lexeme do
    MP.try withClass <|> aux
  where aux = debug "pType[aux]" $ do
            let ~t = MP.try pOperator <|> pType' s
            (:) <$> t <*> MP.many (MP.try s *> t)

        withClass = do
            inst@((n, _):_) <- pHasInstance <* MP.try s
            (pSymbol' "=>" <|> pSymbol' "⇒") <* MP.try s
            ty <- pType s
            pure [locate (TImplements inst ty) (location n)]

pType' :: Parser () -> Parser AType
pType' s = MP.try (pApplication s) <|> (pAtom s)

pApplication :: Parser () -> Parser AType
pApplication s = debug "p[Type]Application" $ do
    withPosition (TApplication <$> types)
  where types = lexeme do
            (:) <$> pAtom s <*> MP.some (MP.try (s *> pAtom s))
