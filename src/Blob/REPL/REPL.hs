{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, LambdaCase, TupleSections, BangPatterns #-}

module Blob.REPL.REPL
( runREPL
, customRunREPL
, replLoop
, REPLError
, REPLOptions(..)
) where

import System.Console.Haskeline (getInputLine, runInputT, InputT, MonadException(..), RunIO(..), defaultSettings, withInterrupt, handleInterrupt)
import Control.Monad.Except (runExceptT, throwError, catchError, MonadError, ExceptT(..), runExcept)
import Control.Monad.State (StateT(..), evalStateT, lift, get, MonadIO, liftIO, runState, modify, evalState, gets)
import Control.Monad.Reader (runReaderT)
import Blob.REPL.Types (REPLError, REPLState(..), REPL, Command(..), REPLOptions(..))
import Blob.Interpreter.Types (Value(..), EvalState(..))
import Blob.Language.TypeChecking.Types (GlobalEnv(..), TypeEnv(..), CustomScheme(..), Scheme(..), apply)
import Blob.Language.Desugaring.Types (Program(..), Statement(..), SugarState(..))
import Blob.Prelude (defaultEnv, initGlobalEnv, initEvalState)
import Blob.Interpreter.Eval (evaluate)
import Blob.REPL.Commands (command)
import Blob.REPL.Execution
import Blob.Language.TypeChecking.Inference
import Blob.Language.KindChecking.Checker (kiType, checkKI, kindInference)
import Blob.Language.Pretty.Inference (pType, pKind)
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Void (Void)
import qualified Data.Text as Text (Text, pack, unpack)
import qualified Data.Text.IO as Text (readFile)
import System.Console.ANSI (setSGR, SGR(..), ColorIntensity(..), Color(..), ConsoleLayer(..), ConsoleIntensity(..))
import Text.Megaparsec.Error (ParseErrorBundle(..), errorBundlePretty, bundleErrors, parseErrorTextPretty)
import Text.Megaparsec (runParser, PosState(..), (<|>), eof, try)
import Text.Megaparsec.Pos (SourcePos(..), unPos, mkPos)
import System.IO (hFlush, stdout)
import Control.Monad (forever, void, forM, replicateM, guard, forM_)
import System.Directory (doesFileExist, getCurrentDirectory, canonicalizePath)
import Data.List.NonEmpty (toList)
import Data.List.Utils (split)
import Data.String.Utils (rstrip, strip)
import Data.Char (toUpper)
import Criterion.Measurement (secs, initializeTime, getTime, measure, runBenchmark)
import Criterion.Measurement.Types (whnf, Measured(..))
import Debug.Trace
import System.IO.Unsafe (unsafePerformIO)
import Text.PrettyPrint.Leijen (text, (<$$>), empty)
import Blob.Language.Desugaring.Defaults
import Blob.Language.Desugaring.Desugarer
import Blob.Language.Parsing.Annotation
import System.FilePath.Posix ((</>))
import Blob.REPL.Defaults
import Blob.REPL.Logger
import System.Directory
import Data.Conf
import Data.Maybe

runREPL :: ([FilePath] -> REPL a) -> IO ()
runREPL = flip customRunREPL (REPLOptions [])

customRunREPL :: ([FilePath] -> REPL a) -> REPLOptions -> IO ()
customRunREPL r opts = do
    initREPL

    let fs = preload opts
    let s = initREPLState
    replState <- configState s

    e <- runExceptT (runStateT (runInputT defaultSettings (r fs)) replState)

    case e of
        Left err -> logError err >> customRunREPL r opts
        Right _ -> pure ()
  where
    configState :: REPLState -> IO REPLState
    configState s = do
        home <- getHomeDirectory
        fe <- doesFileExist (home <> "/.iblob")
        if not fe
        then pure s
        else do
            config <- readConf (home <> "/.iblob")
            setSGR [SetColor Foreground Vivid Green]
                >> putStrLn ("Loaded iBlob configuration from \"" <> (home <> "/.iblob") <> "\"")
                >> setSGR [Reset]
                >> hFlush stdout
            pure $ REPLState (ctx s) (values s) (op s) (fromMaybe "> " (getConf "prompt" config))

replLoop :: [FilePath] -> REPL ()
replLoop fs = do
    let check i f = do
            currentDir <- liftIO getCurrentDirectory
            path <- liftIO $ canonicalizePath (currentDir </> f)
            liftIO $ setSGR [SetColor Foreground Vivid Green] >> putStrLn ("[" <> show i <> " of " <> show (length fs) <> "] Loading file \"" <> path <> "\".") >> setSGR [Reset] >> hFlush stdout
    forM_ (zip [1..] fs) $ \(i, f) -> check i f *> replCheck (Load f)

    forever $ withInterrupt (handleInterrupt run run)
  where
    run = do
        p <- lift (gets prompt)
        input <- getInputLine p

        case input of
            Nothing     -> liftIO exitCommand
            Just input' -> do
                env <- lift get

                let res = runParser command "" input'

                case res of
                    Left err     -> liftIO $
                        if strip input' /= ""
                        then setSGR [SetColor Foreground Vivid Red]
                                >> mapM_ (putStr . parseErrorTextPretty) (toList $ bundleErrors err)
                                >> setSGR [Reset] >> hFlush stdout
                        else putStr ""
                    Right output -> catchError (replCheck output) (liftIO . logError)

    
replCheck :: Command -> REPL ()
replCheck = \case
    Help                -> liftIO helpCommand
    Exit                -> liftIO exitCommand
    ResetEnv ids        -> resetEnv ids
    Load file           -> loadFile file
    GetType expr        -> getType expr
    GetKind typeExpr    -> getKind typeExpr
    Code stat           -> execCode stat
    Time expr           -> execTime expr
    Bench n expr        -> execBench n expr
    Env                 -> getEnv


capitalize :: String -> String
capitalize "" = ""
capitalize (x:xs) = toUpper x : xs











instance MonadException m => MonadException (ExceptT e m) where
    controlIO f = ExceptT $ controlIO $ \(RunIO run) -> let
                    run' = RunIO (fmap ExceptT . run . runExceptT)
                    in runExceptT <$> f run'

instance MonadException m => MonadException (StateT s m) where
    controlIO f = StateT $ \s -> controlIO $ \(RunIO run) -> let
                    run' = RunIO (fmap (StateT . const) . run . flip runStateT s)
                    in flip runStateT s <$> f run'

instance MonadError REPLError (InputT (StateT REPLState (ExceptT REPLError IO))) where
    throwError = lift . lift . throwError
    m `catchError` h = do
        let s = runInputT defaultSettings m
        lift $ s `catchError` (runInputT defaultSettings . h)
