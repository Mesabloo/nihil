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

pPattern :: Parser [APattern]
pPattern = debug "pPattern" $ do
    pos <- getSourcePos
    let t = pOperator <|> pAtom
    term <- (:) <$> t <*> MP.many (sameLineOrIndented pos t) -- MP.some (sameLineOrIndented pos (pOperator <|> pAtom))
    typed <- MP.optional (sameLineOrIndented pos (pSymbol ":") *> sameLineOrIndented pos pType)
    let pat  = maybe term ((: []) . (`locate` pos) . PTypeAnnotated term) typed
    pure pat