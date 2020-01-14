{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}

module Main
( main ) where

import Nihil
import Nihil.Utils.Debug
import Nihil.Utils.Source
import Prelude hiding (log, error)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Text.PrettyPrint.ANSI.Leijen (Doc, text, red)
import System.Exit
import Data.Bifunctor (first)
import qualified Text.Megaparsec as MP
import Control.Monad.Except (ExceptT, runExceptT, liftEither)
import Control.Lens ((%~), (^.))
import Control.Monad.IO.Class (liftIO)

main :: IO ()
main = (runExceptT . workWith =<< T.getContents) >>= \case
    Left err -> error err (print (red err)) *> exitFailure
    Right _  -> exitSuccess

workWith :: T.Text -> ExceptT Doc IO ()
workWith input = do
    let filename = "stdin"
    !lexemes <- log "Lexing code..."       $ liftEither (first mpErrorToDoc (runLexer input filename))
    !ast     <- log "Parsing code..."      $ liftEither (first mpErrorToDoc (runParser lexemes filename))
    !dast    <- log "Desugaring AST..."    $ liftEither (first text (runDesugarer ast))
    _        <- log "Typechecking code..." $ liftEither (runTypeChecker defaultGlobalEnv dast)
    log "Evaluating code..." $ eval defaultEvalEnv dast
  where mpErrorToDoc :: (MP.Stream s, MP.ShowErrorComponent e) => MP.ParseErrorBundle s e -> Doc
        mpErrorToDoc = text . MP.errorBundlePretty

        eval :: EvalState -> Program -> ExceptT Doc IO ()
        eval _ (Program [])       = pure ()
        eval env (Program (s:ss)) = case annotated s of
            FunctionDefinition name ex -> do
                let inEnv = (vals %~ insert (name, lam name (env ^. vals))) env
                val <- liftIO (evaluate ex inEnv)
                val <- liftEither val
                eval ((vals %~ insert (name, val)) inEnv) (Program ss)
            _                          -> eval env (Program ss)

        dummyPos'ed = (`locate` NoSource)
        lam name = VLambda (dummyPos'ed (PId "x")) (dummyPos'ed (EApplication (dummyPos'ed (EId name)) (dummyPos'ed (EId "x"))))