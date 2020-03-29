{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Main
( main ) where

import Nihil
import Nihil.Utils.Debug
import Nihil.Utils.Source
import Nihil.CommonError
import Prelude hiding (log, error, lookup, (!!))
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Text.PrettyPrint.ANSI.Leijen (Doc, text, red, putDoc, hardline)
import qualified Text.PrettyPrint.ANSI.Leijen as PP (pretty)
import qualified Text.Megaparsec as MP
import System.Exit
import Control.Monad.Except (ExceptT, runExceptT, liftEither, throwError)
import Control.Lens ((%~), (^.))
import Control.Monad.IO.Class (liftIO)
import Control.Monad (when)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe (isNothing)
import Data.List.NonEmpty ((!!))

main :: IO ()
main = (runExceptT . workWith =<< T.getContents) >>= \case
    Left err -> error err (putDoc (red err <> hardline)) *> exitFailure
    Right _  -> exitSuccess

workWith :: T.Text -> ExceptT Doc IO ()
workWith input = do
    let filename = "stdin"
        inputLines = T.lines input <> [""]

    let !res = log "Parsing code..."      $ runParser input filename
    !ast <- case res of
        Left err -> throwError (pretty (err `withCode` inputLines))
        Right ast -> pure ast
    !dast    <- log "Desugaring AST..."    $ liftEither (runDesugarer ast)
    info (PP.pretty dast) (pure ())
    log "Typechecking code..."             $ liftEither (runTypeChecker defaultGlobalEnv dast)
    let !env = addToEnv defaultEvalEnv dast

    when (isNothing (lookup "main" (env ^. vals))) do
        throwError (text "Function \"main\" not found.")

    val      <- log "Evaluating code..."   $ liftEither =<< liftIO (evaluate (dummyPos'ed (EId "main")) env)
    info (PP.pretty val) (pure ())
  where addToEnv :: EvalState -> Program -> EvalState
        addToEnv env (Program [])     = env
        addToEnv env (Program (s:ss)) = case annotated s of
            FunctionDefinition name ex -> addToEnv ((vals %~ insert (name, VUnevaluated ex)) env) (Program ss)
            TypeDefinition _ _ cty     -> case annotated cty of
                SumType ctors -> do
                    let newEnv = Set.fromList (Map.keys ctors)
                    addToEnv ((cons %~ (<>) newEnv) env) (Program ss)
                _             -> addToEnv env (Program ss)
            _                          -> addToEnv env (Program ss)

        dummyPos'ed = (`locate` NoSource)
