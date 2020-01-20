module Nihil.Syntax.Concrete.Parser.Pattern.Atom
( pAtom ) where

import Nihil.Syntax.Common (Parser)
import Nihil.Syntax.Concrete.Core
import Nihil.Syntax.Concrete.Parser
import Nihil.Syntax.Concrete.Parser.Identifier
import Nihil.Syntax.Concrete.Parser.Literal
import Nihil.Syntax.Concrete.Parser.Pattern.Tuple
import Nihil.Syntax.Concrete.Parser.Pattern.Wildcard
import Nihil.Syntax.Concrete.Parser.Enclosed
import {-# SOURCE #-} Nihil.Syntax.Concrete.Parser.Pattern
import Nihil.Syntax.Concrete.Debug
import qualified Text.Megaparsec as MP

pAtom :: Parser APattern
pAtom = debug "p[Pattern]Atom" $ withPosition (MP.choice terms)

terms :: [Parser Pattern]
terms =
    [ pWildcard
    , PLiteral             <$> pFloat
    , PLiteral             <$> pInteger
    , PLiteral             <$> pCharacter
    , PLiteral             <$> pString
    , PId                  <$> pIdentifier
    , MP.try pTuple
    , PParens              <$> pParens pPattern
    , flip PConstructor [] <$> pIdentifier' ]