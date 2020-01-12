module Nihil.Syntax
( -- * Lexer
  runLexer
  -- * Parser
, runParser
, module CC
  -- * Desugarer
, runDesugarer
) where

import Nihil.Syntax.Common
import Nihil.Syntax.Concrete.Lexer.Program (lProgram)
import Nihil.Syntax.Concrete.Parser.Statement (pProgram)
import Nihil.Syntax.Concrete.Lexeme
import Nihil.Syntax.Concrete.Core as CC
import qualified Nihil.Syntax.Abstract.Core as AC
import Nihil.Utils.Impossible (impossible)
import Nihil.Syntax.Abstract.Accumulator (accumulateOnProgram)
import Nihil.Syntax.Abstract.Desugarer.Statement (desugarProgram)
import qualified Data.Text as Text
import Data.Void (Void)
import qualified Text.Megaparsec as MP (ParseErrorBundle, runParser, runParserT)
import Control.Monad.State (evalState, evalStateT)
import Control.Monad.Except (runExcept)
import Data.Maybe (mapMaybe)
import qualified Data.Map as Map

{-| Runs the lexer on a @'Text.Text'@ stream, returning either an error, or the tokens got in the stream. -}
runLexer :: Text.Text -> String -> Either (MP.ParseErrorBundle Text.Text Void) [Token]
runLexer input file = evalState (MP.runParserT lProgram file input) initLexerState
  where initLexerState = LState 0

{-| Runs the parser on a @['Token']@ stream, returning either an error, or the AST parsed. -}
runParser :: [Token] -> String -> Either (MP.ParseErrorBundle [ALexeme] Void) Program
runParser input file = MP.runParser pProgram file (toLexemes input)
  where toLexemes = mapMaybe check

        check (Token Nothing) = impossible "filtering non-token when lexing didn't work correctly: non-token found..."
        check (Token lexeme)  = lexeme

{-| Runs the desugarer on a given AST, returning either an error or the new AST desugared. -}
runDesugarer :: CC.Program -> Either String AC.Program
runDesugarer p = runExcept (evalStateT (desugar p) defaultOperators)
  where desugar p = accumulateOnProgram p *> desugarProgram p
        defaultOperators = DState defaultTOps defaultVOps defaultPOps

        defaultTOps = Map.fromList
            [ ("->", (R, 0)), ("→", (R, 0)) ]
        defaultVOps = Map.fromList
            [ ("*", (L, 7)), ("/", (L, 7))
            , ("+", (L, 6)), ("-", (L, 6)) ]
        defaultPOps = Map.fromList
            [ ("Cons", (L, 5)) ]