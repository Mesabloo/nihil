{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}

module Main
( main ) where

import Nihil
import Nihil.Utils.Debug
import Nihil.Utils.Source
import Prelude hiding (log, error, lookup)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Text.PrettyPrint.ANSI.Leijen (Doc, text, red)
import System.Exit
import Control.Monad.Except (ExceptT, runExceptT, liftEither, throwError)
import Control.Lens ((%~), (^.))
import Control.Monad.IO.Class (liftIO)
import Control.Monad (when)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe (isNothing)

main :: IO ()
main = (runExceptT . workWith =<< T.getContents) >>= \case
    Left err -> error err (print (red err)) *> exitFailure
    Right _  -> exitSuccess

workWith :: T.Text -> ExceptT Doc IO ()
workWith input = do
    let filename = "stdin"
    !lexemes <- log "Lexing code..."       $ liftEither (runLexer input filename)
    !ast     <- log "Parsing code..."      $ liftEither (runParser lexemes filename)
    !dast    <- log "Desugaring AST..."    $ liftEither (runDesugarer ast)
    info (pretty dast) (pure ())
    log "Typechecking code..."             $ liftEither (runTypeChecker defaultGlobalEnv dast)
    let !env = addToEnv defaultEvalEnv dast

    when (isNothing (lookup "main" (env ^. vals))) do
        throwError (text "Function \"main\" not found.")

    val      <- log "Evaluating code..."   $ liftEither =<< liftIO (evaluate (dummyPos'ed (EId "main")) env)
    info (pretty val) (pure ())
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