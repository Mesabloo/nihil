{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, LambdaCase #-}

module Blob.REPL.REPL
( runREPL
, customRunREPL
, replLoop
, REPLError
, REPLOptions(..)
) where

import System.Console.Haskeline (getInputLine, runInputT, InputT, MonadException(..), RunIO(..), defaultSettings, withInterrupt, handleInterrupt)
import Control.Monad.Except (runExceptT, throwError, catchError, MonadError, ExceptT(..))
import Control.Monad.State (StateT(..), lift, get, liftIO, gets)
import Blob.REPL.Types (REPLError, REPLState(..), REPL, Command(..), REPLOptions(..))
import Blob.REPL.Commands (command)
import Blob.REPL.Execution
import System.Console.ANSI (setSGR, SGR(..), ColorIntensity(..), Color(..), ConsoleLayer(..))
import Text.Megaparsec.Error (ParseErrorBundle(..), bundleErrors, parseErrorTextPretty)
import Text.Megaparsec (runParser)
import System.IO (hFlush, stdout)
import Control.Monad (forever, forM_)
import System.Directory (doesFileExist, getCurrentDirectory, canonicalizePath, getHomeDirectory)
import Data.List.NonEmpty (toList)
import Data.String.Utils (strip)
import Data.Char (toUpper)
import System.FilePath.Posix ((</>))
import Blob.REPL.Defaults
import Blob.REPL.Logger
import Data.Conf
import Data.Maybe

runREPL :: REPL a -> IO ()
runREPL = flip customRunREPL (REPLOptions [])

customRunREPL :: REPL a -> REPLOptions -> IO ()
customRunREPL r opts = do
    initREPL

    let fs = preloadFiles opts
    let s = initREPLState
    replState <- configState s fs

    e <- runExceptT (runStateT (runInputT defaultSettings r) replState)

    case e of
        Left err -> logError err -- >> customRunREPL r opts
        Right _ -> pure ()
  where
    configState :: REPLState -> [FilePath] -> IO REPLState
    configState s fs = do
        home <- getHomeDirectory
        fe <- doesFileExist (home <> "/.iblob")
        if not fe
        then pure s
        else do
            config <- readConf (home <> "/.iblob")
            setSGR [SetColor Foreground Vivid Green]
                >> putStrLn ("Loaded iBlob configuration from \"" <> (home <> "/.iblob") <> "\".")
                >> setSGR [Reset]
                >> hFlush stdout
            pure $ REPLState (ctx s) (values s) (op s) (fromMaybe "> " (getConf "prompt" config)) (fromMaybe fs (getConf "preload" config))

loadFiles :: REPL ()
loadFiles = do
    let check i f fs = do
            currentDir <- liftIO getCurrentDirectory
            path <- liftIO $ canonicalizePath (currentDir </> f)
            liftIO $ setSGR [SetColor Foreground Vivid Green] >> putStrLn ("[" <> show i <> " of " <> show (length fs) <> "] Loading file \"" <> path <> "\".") >> setSGR [Reset] >> hFlush stdout
    fs <- lift $ gets preload
    forM_ (zip [1..] fs) $ \(i, f) -> check i f fs *> catchError (replCheck (Load f)) (liftIO . logError)

replLoop :: REPL ()
replLoop = do
    loadFiles

    forever $ withInterrupt (handleInterrupt run run)
  where
    run = do
        p <- lift (gets prompt)
        input <- getInputLine p

        case input of
            Nothing     -> liftIO exitCommand
            Just input' -> do
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
    ResetEnv ids        -> resetEnv ids loadFiles
    Load file           -> loadFile file
    GetType expr        -> getType expr
    GetKind typeExpr    -> getKind typeExpr
    Code stat           -> execCode stat
    Time expr           -> execTime expr
    Bench n expr        -> execBench n expr
    Env                 -> getEnv













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
