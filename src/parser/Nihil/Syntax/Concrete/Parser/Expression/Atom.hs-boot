module Nihil.Syntax.Concrete.Parser.Expression.Atom where

import Nihil.Syntax.Common (Parser)
import Nihil.Syntax.Concrete.Core

pAtom :: Parser () -> Parser AAtom
pAtomNoApp :: Parser () -> Parser AAtom
