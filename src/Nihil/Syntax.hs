{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE BangPatterns #-}

module Nihil.Syntax
( -- * Lexer
  runLexer
  -- * Parser
, runParser
  -- * Desugarer
, runDesugarer
  -- * Re-exports
, module Nihil.Syntax.Pretty
, module AC
) where

import Nihil.Syntax.Common
import Nihil.CommonError
import Nihil.Utils.Source
import Nihil.Syntax.Concrete.Parser.Statement (pProgram)
import Nihil.Syntax.Concrete (Token)
import Nihil.Syntax.Concrete.Lexer (runLexer)
import qualified Nihil.Syntax.Concrete.Core as CC
import Nihil.Syntax.Abstract.Core as AC
import Nihil.Syntax.Abstract.Accumulator (accumulateOnProgram)
import Nihil.Syntax.Abstract.Desugarer.Statement (desugarProgram)
import Nihil.Syntax.Pretty
import qualified Text.Megaparsec as MP
import Control.Monad.State (evalStateT)
import Control.Monad.Except (runExcept)
import Data.Void
import qualified Data.Map as Map
import Data.Bifunctor (first)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as T

{-| Runs the parser on a @['Token']@ stream, returning either an error, or the AST parsed. -}
runParser :: [Token] -> String -> Either (Diagnostic T.Text) CC.Program
runParser input file = first megaparsecErrorToCommonError (MP.runParser pProgram file input)

{-| Runs the desugarer on a given AST, returning either an error or the new AST desugared. -}
runDesugarer :: CC.Program -> Either (Diagnostic T.Text) AC.Program
runDesugarer p = runExcept (evalStateT (desugar p) defaultOperators)
  where desugar p = accumulateOnProgram p *> desugarProgram p
        defaultOperators = DState defaultTOps defaultVOps defaultPOps

        defaultTOps = Map.fromList
            [ ("->", (CC.R, 0)), ("→", (CC.R, 0)) ]
        defaultVOps = Map.fromList
            [ ("*", (CC.L, 7)), ("/", (CC.L, 7))
            , ("+", (CC.L, 6)), ("-", (CC.L, 6))
            , ("Cons", (CC.R, 5)) ]
        defaultPOps = Map.fromList
            [ ("Cons", (CC.R, 5)) ]

megaparsecErrorToCommonError :: MP.ParseErrorBundle [Token] Void -> Diagnostic T.Text
megaparsecErrorToCommonError MP.ParseErrorBundle{..} =
    let diag = errorDiagnostic
            `withMessage` "Parse error on input"
            `withBatches` [ newBatch `withLabels` (NonEmpty.toList bundleErrors >>= parseErrorToLabel bundlePosState) ]
    in diag
  where parseErrorToLabel :: MP.PosState [Token] -> MP.ParseError [Token] Void -> [Label]
        parseErrorToLabel pst err =
            let (_, pos)    = MP.reachOffset (MP.errorOffset err) pst
                !source     = fromSourcePos (MP.pstateSourcePos pos)
                msgs        = lines (MP.parseErrorTextPretty err)
            in  if | length msgs == 1 ->
                       [ primaryLabel source `withLabelMessage` (msgs !! 0) ]
                   | length msgs == 2 ->
                       [ primaryLabel source `withLabelMessage` (msgs !! 0)
                       , secondaryLabel source `withLabelMessage` (msgs !! 1) ]
                   | otherwise        ->
                       [ primaryLabel source `withLabelMessage` "unknown parse error" ]
