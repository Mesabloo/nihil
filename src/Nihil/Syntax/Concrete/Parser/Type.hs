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

pType :: Parser [AType]
pType = debug "pType" $ do
    let ~t = MP.try pOperator <|> pType'
    lineFold \s -> do
        (:) <$> t <*> MP.many (MP.try s *> t)

pType' :: Parser AType
pType' = MP.try pApplication <|> pAtom

pApplication :: Parser AType
pApplication = debug "p[Type]Application" $ do
    withPosition (TApplication <$> types)
  where types = lexeme do
            lineFold \s -> do
                (:) <$> pAtom <*> MP.some (MP.try (s *> pAtom))
                -- (:) <$> ty <*> ((:) <$> ty <*> (pAtom `MP.sepBy` MP.try s))
