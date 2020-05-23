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
import Text.PrettyPrint.ANSI.Leijen (Doc, text, hardline)
import qualified Text.PrettyPrint.ANSI.Leijen as PP (pretty)
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
    Left err -> error err (putDoc (err <> hardline)) *> exitFailure
    Right _  -> exitSuccess

workWith :: T.Text -> ExceptT Doc IO ()
workWith input = do
    let filename = "stdin"
        inputLines = T.lines input <> [""]

    let !res = log "Lexing code..."          $ runLexer input filename
    !lex       <- case res of
        Left err  -> throwError (pretty (err `withCode` inputLines))
        Right lex -> info (PP.pretty lex)    $ pure lex
    let !res = log "Parsing code..."         $ runParser lex filename
    !ast       <- case res of
        Left err  -> throwError (pretty (err `withCode` inputLines))
        Right ast -> info (PP.pretty ast)    $ pure ast
    let !res = log "Desugaring AST..."       $ runDesugarer ast
    !dast      <- case res of
        Left err   -> throwError (pretty (err `withCode` inputLines))
        Right dast -> info (PP.pretty dast)  $ pure dast
    (!bindings, _) <- log "Typechecking code..." $ liftEither (runTypeChecker defaultGlobalEnv dast)

    -- There is no current way of passing environments for now. Will deal with that later.
    val      <- log "Evaluating code..."   $ liftEither =<< liftIO (eval (EId "main"))
    info (PP.pretty val) (pure ())
