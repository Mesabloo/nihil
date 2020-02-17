{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TupleSections #-}

module Nihil.Syntax.Concrete.Parser.Statement.TypeDeclaration
( pADT, pGADT, pTypeAlias ) where

import Nihil.Syntax.Common (Parser)
import Nihil.Syntax.Concrete.Core
import Nihil.Syntax.Concrete.Parser
import Nihil.Syntax.Concrete.Parser.Keyword
import Nihil.Syntax.Concrete.Parser.Identifier
import Nihil.Syntax.Concrete.Parser.Type.Atom
import Nihil.Syntax.Concrete.Parser.Type
import Nihil.Utils.Source
import Nihil.Syntax.Concrete.Debug
import qualified Text.Megaparsec as MP
import qualified Data.Map as Map
import Prelude hiding (log)

pADT :: Parser AStatement
pADT = debug "pADT" $ withPosition do
    lineFold \s -> lexeme do
        pKeyword "data"
        name <- annotated <$> (MP.try s *> pIdentifier')
        tvs <- fmap annotated <$> MP.many (MP.try (s *> pIdentifier))

        TypeDefinition name tvs <$> do
            MP.try s *> pSymbol' "="
            withPosition do
                let ~constructor = (,) <$> (annotated <$> pIdentifier') <*> MP.many (MP.try (s *> pAtom s))
                ctors <- constructor `MP.sepBy1` (MP.try s *> pSymbol' "|" <* MP.try s)
                pure (SumType (Map.fromList ctors))

pGADT :: Parser AStatement
pGADT = debug "pGADT" $ withPosition do
    lineFold \spaces -> lexeme do
        pKeyword "data"
        name <- annotated <$> (MP.try spaces *> pIdentifier')
        tvs <- fmap annotated <$> MP.many (MP.try (spaces *> pIdentifier))
        MP.try spaces *> pKeyword "where"

        let ~constructor = lineFold \s -> do
                (,) <$> (annotated <$> pIdentifier') <*> ((MP.try s *> pSymbol' ":") *> (MP.try s *> pType s))

        ctors <- indentBlock1 constructor

        pure (TypeDefinition name tvs (locate (GADT (Map.fromList ctors)) NoSource))

pTypeAlias :: Parser AStatement
pTypeAlias = debug "pTypeAlias" $ withPosition do
    lineFold \s -> do
        pKeyword "type"
        name <- annotated <$> (MP.try s *> pIdentifier')
        tvs  <- fmap annotated <$> MP.many (MP.try s *> pIdentifier)
        MP.try s *> pSymbol' "="
        ty   <- withPosition (TypeAlias <$> (MP.try s *> pType s))
        pure (TypeDefinition name tvs ty)