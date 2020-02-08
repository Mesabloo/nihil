module Nihil.Syntax.Concrete.Parser.Pattern.Atom
( pAtom ) where

import Nihil.Syntax.Common (Parser)
import Nihil.Utils.Source
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
import qualified Data.Text as Text

pAtom :: Parser APattern
pAtom = debug "p[Pattern]Atom" $ withPosition (MP.choice terms)

terms :: [Parser Pattern]
terms =
    [ pWildcard
    , MP.try (PLiteral . LDouble
        . annotated                     <$> pFloat)
    , PLiteral . LInteger . annotated   <$> pInteger
    , PLiteral . LCharacter . annotated <$> pCharacter
    , PLiteral . LString
        . Text.unpack . annotated       <$> pString
    , PId . Text.unpack . annotated     <$> pIdentifier
    , MP.try pTuple
    , PParens              <$> pParens pPattern
    , flip PConstructor []
        . Text.unpack . annotated       <$> pIdentifier' ]