module Nihil.Syntax.Concrete.Parser.Expression where

import Nihil.Syntax.Common (Parser)
import Nihil.Syntax.Concrete.Core
import Nihil.Syntax.Concrete.Parser
import Nihil.Syntax.Concrete.Parser.Identifier (pSymbol)
import Nihil.Syntax.Concrete.Parser.Expression.Atom
import Nihil.Syntax.Concrete.Parser.Expression.Where
import Nihil.Syntax.Concrete.Parser.Type
import Nihil.Utils.Source
import Nihil.Syntax.Concrete.Debug
import qualified Text.Megaparsec as MP

pExpression :: Parser AExpr
pExpression = debug "pExpression" $ do
    pos       <- getSourcePos
    atoms     <- withPosition (MP.some (sameLineOrIndented pos pAtom))
    typed     <- MP.optional (sameLineOrIndented pos (typeAnnotation pos))
    whereBind <- MP.optional (sameLineOrIndented pos pWhere)

    let loc x = locate x pos
    let expr  = maybe atoms (loc . (:[]) . loc . ATypeAnnotated atoms) typed
    let expr' = maybe atoms (loc . (:[]) . loc . AWhere expr) whereBind
    pure expr'
  where typeAnnotation pos = debug "pTypeAnnotation" $ pSymbol ":" *> sameLineOrIndented pos pType