{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Nihil.Syntax.Concrete.Parser.Pattern where

import Nihil.Syntax.Common (Parser)
import Nihil.Syntax.Concrete.Core
import Nihil.Utils.Source
import Nihil.Syntax.Concrete.Parser
import Nihil.Syntax.Concrete.Parser.Pattern.Operator
import Nihil.Syntax.Concrete.Parser.Pattern.Atom
import Nihil.Syntax.Concrete.Parser.Type
import Nihil.Syntax.Concrete.Parser.Identifier
import Nihil.Syntax.Concrete.Debug
import qualified Text.Megaparsec as MP
import Control.Applicative ((<|>))
import qualified Data.Text as Text

pPattern :: Parser [APattern]
pPattern = debug "pPattern" $ lexeme do
    lineFold \s -> do
        let ~t = pOperator <|> MP.try (constructor s) <|> pAtom
        atoms <- (:) <$> t <*> MP.many (MP.try s *> t)

        typed <- MP.optional (MP.try s *> pSymbol' ":" *> MP.try s *> pType)
        let annotate t = [locate (PTypeAnnotated atoms t) NoSource]
        pure (maybe atoms annotate typed)
  where constructor sp =
            withPosition (PConstructor <$> (Text.unpack . annotated <$> pIdentifier')
                                       <*> MP.many (MP.try sp *> pAtom))