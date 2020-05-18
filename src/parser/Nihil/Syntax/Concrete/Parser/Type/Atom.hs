module Nihil.Syntax.Concrete.Parser.Type.Atom
( pAtom ) where

import Nihil.Syntax.Common (Parser)
import Nihil.Syntax.Concrete.Core
import Nihil.Syntax.Concrete.Parser
import Nihil.Syntax.Concrete.Parser.Identifier
import Nihil.Syntax.Concrete.Parser.Type.Tuple
import Nihil.Syntax.Concrete.Parser.Enclosed
import Nihil.Syntax.Concrete.Debug
import Nihil.Utils.Source
import {-# SOURCE #-} Nihil.Syntax.Concrete.Parser.Type
import qualified Text.Megaparsec as MP

pAtom :: Parser () -> Parser AType
pAtom s = debug "p[Type]Atom" $ withPosition (MP.choice (atoms s))

atoms :: Parser () -> [Parser Type]
atoms s =
    [ TVar . annotated <$> pIdentifier MP.<?> "type variable"
    , TId . annotated  <$> pIdentifier' MP.<?> "type identifier"
    , MP.try (pTuple s) MP.<?> "tuple"
    , TParens          <$> pParens (pType s) MP.<?> "parenthesized type"
    ]
