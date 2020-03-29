{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Nihil.Syntax.Concrete.Parser.Expression where

import Nihil.Syntax.Common (Parser)
import Nihil.Syntax.Concrete.Core
import Nihil.Syntax.Concrete.Parser
import Nihil.Syntax.Concrete.Parser.Identifier (pSymbol')
import Nihil.Syntax.Concrete.Parser.Expression.Atom
import Nihil.Syntax.Concrete.Parser.Expression.Where
import Nihil.Syntax.Concrete.Parser.Type
import Nihil.Utils.Source
import Nihil.Syntax.Concrete.Debug
import qualified Text.Megaparsec as MP

pExpression :: Parser () -> Parser Expr
pExpression s = debug "pExpression" $ lexeme do
    atoms <- pAtom s `MP.sepBy1` MP.try s
    MP.try space1
    typed <- MP.optional (typeAnnotation s)
    whereB <- MP.optional (pWhere s <* MP.try space1)

    let annotate t = [locate (ATypeAnnotated atoms t) NoSource]
    let expr = maybe atoms annotate typed
        where' ss = [locate (AWhere expr ss) NoSource]
    pure (maybe expr where' whereB)
  where typeAnnotation sp = debug "pTypeAnnotation" $ pSymbol' ":" *> MP.try sp *> pType sp <* MP.try space1
