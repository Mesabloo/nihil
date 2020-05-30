{-# LANGUAGE BlockArguments #-}

module Nihil.Syntax.Concrete.Parser.Statement where

import Nihil.Syntax.Common (Parser)
import Nihil.Syntax.Concrete.Core
import Nihil.Syntax.Concrete.Parser
import Nihil.Utils.Source
import Nihil.Syntax.Concrete.Parser.Statement.FunctionDeclaration
import Nihil.Syntax.Concrete.Parser.Statement.OperatorFixity
import Nihil.Syntax.Concrete.Parser.Statement.TypeDeclaration
import qualified Text.Megaparsec as MP

pProgram :: Parser Program
pProgram = Program <$> MP.many (lexeme pStatement) <* eof

pStatement :: Parser AStatement
pStatement = nonIndented do
    MP.choice
        [ pOperatorFixity
        , pTypeAlias
        , MP.try pADT
        , pGADT
        , MP.try pFunctionDeclaration
        , pFunctionDefinition ]

eof :: Parser ()
eof = () <$ MP.satisfy ((== TkEOF) . annotated)
