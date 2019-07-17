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
import Blob.TypeChecking.Types (GlobalEnv(..), TypeEnv(..), CustomScheme(..), Scheme(..), apply)
import Blob.Desugaring.Types (Program(..), Statement(..), SugarState(..))
import Blob.Prelude (defaultEnv, initGlobalEnv, initEvalState)
import Blob.Interpreter.Eval (evaluate)
import Blob.REPL.Commands (command)
import Blob.REPL.Execution
import Blob.Parsing.Parser (parseProgram, parseStatement)
import Blob.TypeChecking.Inference
import Blob.KindChecking.Checker (kiType, checkKI, kindInference)
import Blob.Pretty.Parser (pExpression, pStatement, pProgram)
import Blob.Pretty.Inference (pType, pKind)
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Void (Void)
import qualified Data.Text as Text (Text, pack)
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
import Blob.Desugaring.Defaults
import Blob.Desugaring.Desugarer
import Blob.Parsing.Annotation
import System.FilePath.Posix ((</>))
import Blob.REPL.Defaults

runREPL :: ([FilePath] -> REPL a) -> IO ()
runREPL = flip customRunREPL (REPLOptions [])

customRunREPL :: ([FilePath] -> REPL a) -> REPLOptions -> IO ()
customRunREPL r opts = do
    initREPL

    let fs = preload opts
    e <- runExceptT (runStateT (runInputT defaultSettings (r fs)) initREPLState)

    case e of
        Left err -> setSGR [SetColor Foreground Vivid Red] >> putStr (show err) >> setSGR [Reset] >> hFlush stdout
                    >> customRunREPL r opts
        Right _ -> pure ()

replLoop :: [FilePath] -> REPL ()
replLoop fs = do
    if null fs
    then liftIO $ putStr ""
    else liftIO $ putStrLn ""

    let check i f = do
            currentDir <- liftIO getCurrentDirectory
            path <- liftIO $ canonicalizePath (currentDir </> f)
            liftIO $ setSGR [SetColor Foreground Vivid Green] >> putStrLn ("[" <> show i <> " of " <> show (length fs) <> "] Compiling file “" <> path <> "”.") >> setSGR [Reset] >> hFlush stdout
    forM_ (zip [1..] fs) $ \(i, f) -> check i f *> replCheck (Load f)
    
    if null fs
    then liftIO $ putStr ""
    else liftIO $ putStrLn ""

    forever $ withInterrupt (handleInterrupt run run)
  where
    run = do
        time <- lift (gets lastExecTime)
        let str = if time /= 0.0
                then secs time <> " "
                else ""

        liftIO $ setSGR [SetColor Foreground Dull Blue]
        input <- getInputLine $ str <> "⮞ β ⮞ "
        liftIO $ setSGR [Reset]

        lift . modify $ \st -> st { lastExecTime = 0.0 }

        case input of
            Nothing     -> liftIO exitCommand
            Just input' -> do
                env <- lift get

                let res = runParser command "" (Text.pack input')

                case res of
                    Left err     -> liftIO $
                        if strip input' /= ""
                        then setSGR [SetColor Foreground Vivid Red]
                                >> mapM_ (putStr . parseErrorTextPretty) (toList $ bundleErrors err)
                                >> setSGR [Reset] >> hFlush stdout
                        else putStr ""
                    Right output -> catchError (replCheck output)
                                    (\e -> liftIO $ setSGR [SetColor Foreground Vivid Red]
                                                    >> putStr (show e)
                                                    >> setSGR [Reset] >> hFlush stdout)

    
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



replErrorPretty :: ParseErrorBundle Text.Text Void -> IO ()
replErrorPretty bundle = do
    let (PosState _ _ state _ _) = bundlePosState bundle
        errors = map (, state) (toList $ bundleErrors bundle)
        texts  = flip List.map errors $ \(e, pos) -> do let (SourcePos name line col) = pos
                                                        "at <“" <> name <> "”:" <> show (unPos line) <> ":" <> show (unPos col) <> ">\n" <> parseErrorTextPretty e
        lines_ = List.map (split "\n") texts
        lines' = List.map (filter (/= "")) lines_
        errs   = List.map (List.map (capitalize . rstrip . flip (<>) ".")) lines'
        errs'  = List.map (List.intercalate "\n") errs
    mapM_ putStr errs' >> putStr "\n"


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
