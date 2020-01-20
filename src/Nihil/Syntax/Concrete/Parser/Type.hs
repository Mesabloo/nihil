module Nihil.Syntax.Concrete.Parser.Type
( pType ) where

import Nihil.Syntax.Common (Parser)
import Nihil.Syntax.Concrete.Core
import Nihil.Syntax.Concrete.Parser.Type.Operator
import Nihil.Syntax.Concrete.Parser.Type.Atom
import Nihil.Syntax.Concrete.Parser
import Nihil.Syntax.Concrete.Debug
import qualified Text.Megaparsec as MP
import Control.Applicative ((<|>))

pType :: Parser [AType]
pType = debug "pType" $ do
    pos <- getSourcePos
    let t = pOperator <|> pType'
    (:) <$> t <*> MP.many (sameLineOrIndented pos t) -- MP.some (pOperator <|> pType')

pType' :: Parser AType
pType' = MP.try pApplication <|> pAtom

pApplication :: Parser AType
pApplication = do
    pos <- getSourcePos
    withPosition (TApplication <$> types pos)
  where types pos = (:) <$> pAtom
                        <*> MP.some (sameLineOrIndented pos pAtom)