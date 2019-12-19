-- The Great Nihil Compiler
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

module Nihil.Interactive where

import Nihil.Interactive.Command (CommandParser, Command(..))
import Nihil.Interactive.REPL (REPL, REPLState, REPLOptions(..), REPLError, preload, prompt, preloadFiles)
import Nihil.Interactive.Commands.Bench
import Nihil.Interactive.Commands.Code
import Nihil.Interactive.Commands.Exit
import Nihil.Interactive.Commands.GetEnv
import Nihil.Interactive.Commands.GetKind
import Nihil.Interactive.Commands.GetType
import Nihil.Interactive.Commands.Help
import Nihil.Interactive.Commands.Load
import Nihil.Interactive.Commands.Reset
import Nihil.Interactive.Commands.Shell
import Nihil.Interactive.Commands.Time
import Nihil.Interactive.Defaults
import Nihil.Interactive.Commands.Errors.UnknownCommand (makeCommandError)
import Text.Megaparsec (observing, try, choice, parseErrorTextPretty, runParser, ParseErrorBundle(..), bundleErrors, (<|>), eof)
import qualified Text.Megaparsec.Char as C
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
import Text.PrettyPrint.ANSI.Leijen (text, green, red)

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
    -- ^ Updates a 'REPLState' according to the file `$HOME/.nihili`, if existing.
    configState s fs = do
        home <- getHomeDirectory
        fe <- doesFileExist (home <> "/.nihili")
        if not fe
        then pure s
        else do
            config <- readConf (home <> "/.nihili")
            print (green . text $ "Loaded Nihili configuration from \"" <> (home <> "/.nihili") <> "\".")
            pure $ s & prompt .~ fromMaybe "> " (getConf "prompt" config)
                     & preload .~ (fromMaybe [] (getConf "preload" config) <> fs)

-- | Loads some files in the REPL.
loadFiles :: REPL ()
loadFiles = do
    let check i f fs = do
            currentDir <- liftIO getCurrentDirectory
            path <- liftIO $ canonicalizePath (currentDir </> f)
            liftIO $ print (green . text $ "[" <> show i <> " of " <> show (length fs) <> "] Loading file \"" <> path <> "\".")
    fs <- use preload
    forM_ (zip [1..] fs) $ \(i, f) -> check i f fs *> catchError (replCheck (Load f)) (liftIO . logError)

logError :: REPLError -> IO ()
logError = putStr . show . red

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
                        then mapM_ (putStr . show . red . text . parseErrorTextPretty) (toList $ bundleErrors err)
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