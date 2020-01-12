module Nihil.Syntax.Concrete.Parser.Type.Atom
( pAtom ) where

import Nihil.Syntax.Common (Parser)
import Nihil.Syntax.Concrete.Core
import Nihil.Syntax.Concrete.Parser
import Nihil.Syntax.Concrete.Parser.Identifier
import Nihil.Syntax.Concrete.Parser.Type.Tuple
import Nihil.Syntax.Concrete.Parser.Enclosed
import Nihil.Syntax.Concrete.Debug
import {-# SOURCE #-} Nihil.Syntax.Concrete.Parser.Type
import qualified Text.Megaparsec as MP

pAtom :: Parser AType
pAtom = debug "p[Type]Atom" $ withPosition (MP.choice atoms)

atoms :: [Parser Type]
atoms =
    [ TVar    <$> pIdentifier
    , TId     <$> pIdentifier'
    , MP.try pTuple
    , TParens <$> pParens pType ]