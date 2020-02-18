{-# LANGUAGE BlockArguments #-}

module Nihil.Syntax.Concrete.Parser.Statement where

import Nihil.Syntax.Common (Parser)
import Nihil.Syntax.Concrete.Core
import Nihil.Syntax.Concrete.Parser
import Nihil.Syntax.Concrete.Parser.Statement.FunctionDeclaration
import Nihil.Syntax.Concrete.Parser.Statement.OperatorFixity
import Nihil.Syntax.Concrete.Parser.Statement.TypeDeclaration
import Nihil.Syntax.Concrete.Parser.Statement.ClassDeclaration
import Nihil.Syntax.Concrete.Parser.Statement.InstanceDeclaration
import qualified Text.Megaparsec as MP

pProgram :: Parser Program
pProgram = Program <$> MP.some (lexemeN pStatement) <* MP.eof

pStatement :: Parser AStatement
pStatement = nonIndented do
    MP.choice
        [ pOperatorFixity
        , pTypeAlias
        , MP.try pADT
        , pGADT
        , pTypeClass
        , pInstanceDeclaration
        , MP.try pFunctionDeclaration
        , pFunctionDefinition ]