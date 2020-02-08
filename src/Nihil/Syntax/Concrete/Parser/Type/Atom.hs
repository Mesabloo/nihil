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
import qualified Data.Text as Text

pAtom :: Parser AType
pAtom = debug "p[Type]Atom" $ withPosition (MP.choice atoms)

atoms :: [Parser Type]
atoms =
    [ TVar . Text.unpack . annotated <$> pIdentifier
    , TId . Text.unpack . annotated  <$> pIdentifier'
    , MP.try pTuple
    , TParens                        <$> pParens pType ]