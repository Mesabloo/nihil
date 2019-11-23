-- Blobc, a compiler for compiling Blob source code
-- Copyright (c) 2019 Mesabloo

-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.

-- You should have received a copy of the GNU General Public License
-- along with this program. If not, see <http://www.gnu.org/licenses/>.

{-# LANGUAGE LambdaCase #-}

module Blob.Interactive where

import Blob.Interactive.Command (CommandParser, Command(..))
import Blob.Interactive.REPL (REPL, REPLState, REPLOptions(..), REPLError, preload, prompt, preloadFiles)
import Blob.Interactive.Commands.Bench
import Blob.Interactive.Commands.Code
import Blob.Interactive.Commands.Exit
import Blob.Interactive.Commands.GetEnv
import Blob.Interactive.Commands.GetKind
import Blob.Interactive.Commands.GetType
import Blob.Interactive.Commands.Help
import Blob.Interactive.Commands.Load
import Blob.Interactive.Commands.Reset
import Blob.Interactive.Commands.Shell
import Blob.Interactive.Commands.Time
import Blob.Interactive.Defaults
import Blob.Interactive.Commands.Errors.UnknownCommand (makeCommandError)
import Text.Megaparsec (observing, try, choice, parseErrorTextPretty, runParser, ParseErrorBundle(..), bundleErrors, (<|>), eof)
import qualified Text.Megaparsec.Char as C
import System.IO (hFlush, stdout)
import Control.Monad (forever, forM_)
import System.Directory (doesFileExist, getCurrentDirectory, canonicalizePath, getHomeDirectory)
import Data.List.NonEmpty (toList)
import Data.String.Utils (strip)
import System.FilePath.Posix ((</>))
import Data.Conf
import Data.Maybe
import Control.Lens
import System.Console.Haskeline (getInputLine, runInputT, defaultSettings, withInterrupt, handleInterrupt)
import Control.Monad.Except (runExceptT, catchError)
import Control.Monad.State (StateT(..), liftIO)
import Data.List (isInfixOf)
import System.Console.ANSI

-- | Runs the REPL with no options.
runREPL :: REPL a -> IO ()
runREPL = flip customRunREPL (REPLOptions [])

-- | Runs the REPL with some specified options
customRunREPL :: REPL a -> REPLOptions -> IO ()
customRunREPL r opts = do
    initREPL

    let fs = opts ^. preloadFiles
    let s = initREPLState
    replState <- configState s fs

    e <- runExceptT (runStateT (runInputT defaultSettings r) replState)

    case e of
        Left err -> logError err -- >> customRunREPL r opts
        Right _ -> pure ()
  where
    configState :: REPLState -> [FilePath] -> IO REPLState
    -- ^ Updates a 'REPLState' according to the file `$HOME/.iblob`, if existing.
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
            pure $ s & prompt .~ fromMaybe "> " (getConf "prompt" config)
                     & preload .~ (fromMaybe [] (getConf "preload" config) <> fs)

-- | Loads some files in the REPL.
loadFiles :: REPL ()
loadFiles = do
    let check i f fs = do
            currentDir <- liftIO getCurrentDirectory
            path <- liftIO $ canonicalizePath (currentDir </> f)
            liftIO $ setSGR [SetColor Foreground Vivid Green] >> putStrLn ("[" <> show i <> " of " <> show (length fs) <> "] Loading file \"" <> path <> "\".") >> setSGR [Reset] >> hFlush stdout
    fs <- use preload
    forM_ (zip [1..] fs) $ \(i, f) -> check i f fs *> catchError (replCheck (Load f)) (liftIO . logError)

logError :: REPLError -> IO ()
logError err = do
    setSGR [SetColor Foreground Vivid Red]
    putStr (show err)
    setSGR [Reset]
    hFlush stdout

-- | The basic loop of the REPL.
replLoop :: REPL ()
replLoop = do
    loadFiles

    forever $ withInterrupt (handleInterrupt replLoop run)
  where
    run = do
        p <- use prompt
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

-- | The REPL command handler.
replCheck :: Command -> REPL ()
replCheck = \case
    Help                -> liftIO helpCommand
    Exit                -> liftIO exitCommand
    ResetEnv ids        -> resetEnv ids loadFiles
    Load file           -> loadFile file
    GetType expr        -> getType' expr
    GetKind typeExpr    -> getKind' typeExpr
    Code stat           -> execCode stat
    Time expr           -> execTime expr
    Bench n expr        -> execBench n expr
    Env                 -> getEnv
    Shell c             -> shell' c

-- | The global command parser.
command :: CommandParser Command
command =
    do
        try (C.space *> C.string ":")
        cmd <- observing . try $ choice [help, exit, load, time, getType, getKind, reset, bench, env, shell] <* eof
        case cmd of
            Left err ->
                if "߷" `isInfixOf` parseErrorTextPretty err
                then do
                    msg <- makeCommandError
                    fail msg
                else fail $ parseErrorTextPretty err
            Right x  -> pure x
    <|> C.space *> code