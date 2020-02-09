{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}

module Nihil.Syntax.Concrete.Parser.Statement.TypeDeclaration
( pDataType, pTypeAlias ) where

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

pDataType :: Parser AStatement
pDataType = debug "pDataType" $ withPosition do
    pKeyword "data"
    lineFold \spaces -> lexeme do
        name <- annotated <$> (MP.try spaces *> pIdentifier')
        tvs <- fmap annotated <$> MP.many (MP.try spaces *> pIdentifier)
        MP.try spaces *> (TypeDefinition name tvs <$> MP.choice
            [ pADT spaces, pGADT spaces ])

pADT :: Parser () -> Parser ACustomType
pADT s = debug "pADT" $ withPosition do
    MP.try s *> pSymbol' "="
    let ~constructor = (,) <$> (annotated <$> pIdentifier') <*> MP.many (MP.try s *> pAtom s)
    ctors <- constructor `MP.sepBy1` (MP.try s *> pSymbol' "|" <* MP.try s)
    pure (SumType (Map.fromList ctors))

pGADT :: Parser () -> Parser ACustomType
pGADT s = debug "pGADT" $ withPosition do
    indentBlock do
        let ~constructor = lineFold \s -> do
                (,) <$> (annotated <$> pIdentifier') <*> ((MP.try s *> pSymbol' ":") *> (MP.try s *> pType s))

        pKeyword "where"
        pure (IndentSome Nothing (pure . GADT . Map.fromList) constructor)

pTypeAlias :: Parser AStatement
pTypeAlias = debug "pTypeAlias" $ withPosition do
    lineFold \s -> do
        pKeyword "type"
        name <- annotated <$> (MP.try s *> pIdentifier')
        tvs  <- fmap annotated <$> MP.many (MP.try s *> pIdentifier)
        MP.try s *> pSymbol' "="
        ty   <- withPosition (TypeAlias <$> (MP.try s *> pType s))
        pure (TypeDefinition name tvs ty)