{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}

module Nihil.Syntax.Concrete.Parser.Type.Has
( pHasInstance ) where

import Nihil.Syntax.Common (Parser)
import Nihil.Syntax.Concrete.Parser (lexemeN, lineFold)
import Nihil.Utils.Source
import Nihil.Syntax.Concrete.Core
import Nihil.Syntax.Concrete.Parser.Identifier
import Nihil.Syntax.Concrete.Parser.Enclosed
import Nihil.Syntax.Concrete.Parser.Type.Atom
import qualified Text.Megaparsec as MP
import Control.Applicative ((<|>))

pHasInstance :: Parser [(Located String, [AType])]
pHasInstance =
    ((:[]) <$> pInstance)
    <|> pParens ((:) <$> (lexemeN pInstance <* lexemeN (pSymbol' ",")) <*> (lexemeN pInstance `MP.sepBy1` lexemeN (pSymbol' ",")))

pInstance :: Parser (Located String, [AType])
pInstance = lineFold \s -> (,) <$> (pIdentifier' <* MP.try s) <*> MP.some (pAtom s)