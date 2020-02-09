{-# LANGUAGE BlockArguments #-}

module Nihil.Syntax.Concrete.Parser.Type
( pType ) where

import Nihil.Syntax.Common (Parser)
import Nihil.Syntax.Concrete.Core
import Nihil.Syntax.Concrete.Parser.Type.Operator
import Nihil.Syntax.Concrete.Parser.Type.Atom
import Nihil.Syntax.Concrete.Parser
import Nihil.Syntax.Concrete.Debug
import qualified Text.Megaparsec as MP
import Control.Applicative ((<|>))

pType :: Parser () -> Parser [AType]
pType s = debug "pType" $ lexeme do
    let ~t = MP.try pOperator <|> pType' s
    (:) <$> t <*> MP.many (MP.try s *> t)

pType' :: Parser () -> Parser AType
pType' s = MP.try (pApplication s) <|> (pAtom s)

pApplication :: Parser () -> Parser AType
pApplication s = debug "p[Type]Application" $ do
    withPosition (TApplication <$> types)
  where types = lexeme do
            (:) <$> pAtom s <*> MP.some (MP.try (s *> pAtom s))
