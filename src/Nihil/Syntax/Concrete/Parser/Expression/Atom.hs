{-# LANGUAGE BlockArguments #-}

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
import Nihil.Utils.Source
import Nihil.Utils.Annotation
import Control.Applicative ((<|>))
import Data.Bifunctor (first)
import qualified Text.Megaparsec as MP
import qualified Data.Text as Text

pAtom :: Parser AAtom
pAtom = debug "p[Expression]Atom" $ MP.try pOperator <|> pAtom'

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
            , AId . annotated      <$> MP.choice
                [ hoist <$> pIdentifier
                , MP.try (pParens pAnySymbolᵉ)
                , hoist <$> pIdentifier' ] MP.<?> "identifier"
            , ALiteral . annotated <$> MP.try pFloat
            , ALiteral . annotated <$> pInteger
            , ALiteral . annotated <$> pCharacter
            , ALiteral . annotated <$> pString
            , AParens              <$> pParens pExpression ]

        hoist = hoistAnnotated (first Text.unpack)

pApplication :: Parser AAtom
pApplication = lexeme do
    withPosition (AApplication <$> exprs)
  where exprs = lineFold \s -> do
            (:) <$> pAtomNoApp <*> MP.some (MP.try (s *> pAtomNoApp))