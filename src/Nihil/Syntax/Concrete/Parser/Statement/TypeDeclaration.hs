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
import Nihil.Syntax.Concrete.Parser.Enclosed
import Nihil.Utils.Source
import Nihil.Syntax.Concrete.Debug
import Nihil.Utils.Debug
import qualified Text.Megaparsec as MP
import qualified Data.Map as Map
import Control.Applicative ((<|>))
import Prelude hiding (log)

pDataType :: Parser AStatement
pDataType = debug "pDataType" $ withPosition do
    pKeyword "data"
    pos  <- getSourcePos
    name <- sameLineOrIndented pos pIdentifier'
    tvs  <- MP.many (sameLineOrIndented pos pIdentifier)

    cTy  <- MP.choice
        [ sameLineOrIndented pos (pSymbol "=") *> pADT pos
        , sameLineOrIndented pos (pKeyword "where") *> (pBraces (pGADT pos) <|> pGADT pos) ]
    pure (TypeDefinition name tvs cTy)

pADT :: SourcePos -> Parser ACustomType
pADT initPos = debug "pADT" $ withPosition do
    let ~constructor = (,) <$> pIdentifier' <*> MP.many (sameLineOrIndented initPos pAtom)
    log "pADT test" (pure ())
    ctors <- sameLineOrIndented initPos constructor `MP.sepBy1` sameLineOrIndented initPos (pSymbol "|")
    pure (SumType (Map.fromList ctors))

pGADT :: SourcePos -> Parser ACustomType
pGADT initPos = debug "pGADT" $ withPosition do
    pos <- getSourcePos
    let ~constructor = (,) <$> sameLineOrIndented initPos pIdentifier' <*> (sameLineOrIndented initPos (pSymbol ":") *> sameLineOrIndented initPos pType)
    ctors <- MP.some (sameLineOrColumn pos constructor <* MP.many (pSymbol ";"))
    pure (GADT (Map.fromList ctors))

pTypeAlias :: Parser AStatement
pTypeAlias = debug "pTypeAlias" $ withPosition do
    pos  <- getSourcePos
    pKeyword "type"
    name <- sameLineOrIndented pos pIdentifier'
    tvs  <- MP.many (sameLineOrIndented pos pIdentifier)
    sameLineOrIndented pos (pSymbol "=")
    ty   <- withPosition (TypeAlias <$> sameLineOrIndented pos pType)
    pure (TypeDefinition name tvs ty)