module Nihil.Syntax.Concrete.Parser.Expression.Atom
( pAtom, pAtomNoApp ) where

import Nihil.Syntax.Common (Parser)
import Nihil.Syntax.Concrete.Core
import Nihil.Syntax.Concrete.Parser
import Nihil.Syntax.Concrete.Parser.Expression.Operator
import Nihil.Syntax.Concrete.Parser.Identifier
import Nihil.Syntax.Concrete.Parser.Enclosed
import Nihil.Syntax.Concrete.Parser.Literal
import Nihil.Syntax.Concrete.Parser.Expression.Lambda
import Nihil.Syntax.Concrete.Parser.Expression.Let
import Nihil.Syntax.Concrete.Parser.Expression.Match
import Nihil.Syntax.Concrete.Parser.Expression.Tuple
import Nihil.Syntax.Concrete.Parser.Expression.TypeHole
import {-# SOURCE #-} Nihil.Syntax.Concrete.Parser.Expression
import Nihil.Syntax.Concrete.Debug
import Control.Applicative ((<|>))
import qualified Text.Megaparsec as MP

pAtom :: Parser AAtom
pAtom = debug "p[Expression]Atom" $ pOperator <|> pAtom'

pAtom' :: Parser AAtom
pAtom' = MP.try pApplication <|> pAtomNoApp

pAtomNoApp :: Parser AAtom
pAtomNoApp = withPosition (MP.choice atoms)
  where atoms =
            [ pTypeHole
            , pLambda
            , pMatch
            , MP.try pTuple
            , pLet
            , AId      <$> MP.choice
                [ pIdentifier
                , MP.try (pParens pAnySymbolᵉ)
                , pIdentifier' ] MP.<?> "identifier"
            , ALiteral <$> pFloat
            , ALiteral <$> pInteger
            , ALiteral <$> pCharacter
            , ALiteral <$> pString
            , AParens  <$> pParens pExpression ]

pApplication :: Parser AAtom
pApplication = do
    pos <- getSourcePos
    withPosition (AApplication <$> exprs pos)
  where exprs pos = (:) <$> pAtomNoApp
                        <*> MP.some (sameLineOrIndented pos pAtomNoApp)