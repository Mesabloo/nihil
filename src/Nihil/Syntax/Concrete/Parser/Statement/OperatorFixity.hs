{-# LANGUAGE BlockArguments #-}

module Nihil.Syntax.Concrete.Parser.Statement.OperatorFixity where

import Nihil.Syntax.Common (Parser)
import Nihil.Syntax.Concrete.Core
import Nihil.Syntax.Concrete.Parser
import Nihil.Syntax.Concrete.Parser.Enclosed
import Nihil.Syntax.Concrete.Parser.Identifier
import Nihil.Syntax.Concrete.Parser.Literal
import Nihil.Syntax.Concrete.Parser.Keyword
import Nihil.Utils.Source
import Nihil.Syntax.Concrete.Debug
import Control.Applicative ((<|>))
import Control.Monad (guard)
import qualified Text.Megaparsec as MP

pOperatorFixity :: Parser AStatement
pOperatorFixity = debug "pOperatorFixity" $ withPosition do
    pos           <- getSourcePos
    f             <- fixity
    LInteger prec <- sameLineOrIndented pos pInteger
    guard (prec < 10 && prec >= 0)
        <|> fail "Operator precedence should be lower than 10 and positive"
    operator      <- sameLineOrIndented pos (pParens op <|> op)
    pure (OperatorFixity operator (locate (f prec) pos))
  where op     = pTicks pIdentifier <|> pAnySymbolᵉ <|> pAnySymbolᵗ
        fixity = Infix <$> MP.choice
            [ L <$ pKeyword "infixl"
            , R <$ pKeyword "infixr" ]