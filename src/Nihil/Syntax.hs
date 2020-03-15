module Nihil.Syntax
( -- * Parser
  runParser
  -- * Desugarer
, runDesugarer
  -- * Re-exports
, module Nihil.Syntax.Pretty
, module AC
) where

import Nihil.Syntax.Common
import Nihil.Syntax.Concrete.Parser.Statement (pProgram)
import qualified Nihil.Syntax.Concrete.Core as CC
import Nihil.Syntax.Abstract.Core as AC
import Nihil.Syntax.Abstract.Accumulator (accumulateOnProgram)
import Nihil.Syntax.Abstract.Desugarer.Statement (desugarProgram)
import Nihil.Syntax.Pretty
import qualified Data.Text as Text
import qualified Text.Megaparsec as MP (runParser, errorBundlePretty)
import Control.Monad.State (evalStateT)
import Control.Monad.Except (runExcept)
import qualified Data.Map as Map
import Data.Bifunctor (first)
import Text.PrettyPrint.ANSI.Leijen (Doc, text)

{-| Runs the parser on a @['Token']@ stream, returning either an error, or the AST parsed. -}
runParser :: Text.Text -> String -> Either Doc CC.Program
runParser input file = first (text . MP.errorBundlePretty) (MP.runParser pProgram file input)

{-| Runs the desugarer on a given AST, returning either an error or the new AST desugared. -}
runDesugarer :: CC.Program -> Either Doc AC.Program
runDesugarer p = runExcept (evalStateT (desugar p) defaultOperators)
  where desugar p = accumulateOnProgram p *> desugarProgram p
        defaultOperators = DState defaultTOps defaultVOps defaultPOps

        defaultTOps = Map.fromList
            [ ("->", (CC.R, -1)), ("→", (CC.R, -1))
            , ("=>", (CC.R, -1)), ("̄̄⇒", (CC.R, -1))
            , ("~", (CC.L, 0)),   ("∼", (CC.L, 0))  ]
        defaultVOps = Map.fromList
            [ ("*", (CC.L, 7)), ("/", (CC.L, 7))
            , ("+", (CC.L, 6)), ("-", (CC.L, 6))
            , ("Cons", (CC.R, 5)) ]
        defaultPOps = Map.fromList
            [ ("Cons", (CC.R, 5)) ]
