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
        s
        name <- annotated <$> pIdentifier'
        tvs <- fmap annotated <$> (pIdentifier `MP.sepBy` s)

        TypeDefinition name tvs <$> do
            s *> pSymbol' "="
            withPosition do
                let ~constructor = lexeme ((,) <$> (annotated <$> pIdentifier' <* s) <*> (pAtom s `MP.sepBy` s)) MP.<?> "datatype constructor"
                ctors <- constructor `MP.sepBy1` (pSymbol' "|" <* s)
                pure (SumType (Map.fromList ctors))

pGADT :: Parser AStatement
pGADT = debug "pGADT" $ withPosition do
    lineFold \spaces -> lexeme do
        pKeyword "data"
        spaces
        name <- annotated <$> pIdentifier'
        tvs <- fmap annotated <$> (pIdentifier `MP.sepBy` spaces)
        spaces *> pKeyword "where"

        let ~constructor = lineFold \s -> lexeme do
                (,) <$> (annotated <$> pIdentifier') <*> ((s *> pSymbol' ":") *> (s *> pType s)) MP.<?> "GADT constructor"

        ctors <- indentBlock constructor

        pure (TypeDefinition name tvs (locate (GADT (Map.fromList ctors)) NoSource))

pTypeAlias :: Parser AStatement
pTypeAlias = debug "pTypeAlias" $ withPosition do
    lineFold \s -> do
        pKeyword "type"
        s
        name <- annotated <$> pIdentifier'
        s
        tvs  <- fmap annotated <$> (pIdentifier `MP.endBy` s)
        pSymbol' "="
        ty   <- withPosition (TypeAlias <$> (s *> pType s))
        pure (TypeDefinition name tvs ty)
