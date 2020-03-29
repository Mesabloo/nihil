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
import Nihil.Syntax.Concrete.Parser.Expression.Record
import {-# SOURCE #-} Nihil.Syntax.Concrete.Parser.Expression
import Nihil.Syntax.Concrete.Debug
import Nihil.Utils.Source
import Control.Applicative ((<|>))
import qualified Text.Megaparsec as MP

pAtom :: Parser () -> Parser AAtom
pAtom s = debug "p[Expression]Atom" $ MP.try pOperator <|> pAtom' s

pAtom' :: Parser () -> Parser AAtom
pAtom' s = MP.try (pApplication s) <|> pAtomNoApp s

pAtomNoApp :: Parser () -> Parser AAtom
pAtomNoApp s = withPosition (MP.choice atoms)
  where atoms =
            [ pTypeHole
            , pLambda s
            , pMatch s
            , pRecord s
            , MP.try (pTuple s)
            , pLet s
            , AId . annotated      <$> MP.choice
                [ pIdentifier
                , MP.try (pParens pAnySymbolᵉ)
                ] MP.<?> "identifier"
            , ALiteral . annotated <$> MP.try pFloat
            , ALiteral . annotated <$> pInteger
            , ALiteral . annotated <$> pCharacter
            , ALiteral . annotated <$> pString
            , AParens              <$> pParens (pExpression s) ]

pApplication :: Parser () -> Parser AAtom
pApplication s = lexeme do
    withPosition (AApplication <$> exprs)
  where exprs = (:) <$> pAtomNoApp s <*> MP.some (MP.try (s *> pAtomNoApp s))
