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

module Main
(
    main
) where

import Blob.Interactive (customRunREPL, replLoop, replCheck)
import Blob.Interactive.REPL (REPLOptions(..))
import Blob.Interactive.Command (Command(Code))
import qualified Blob.Interactive.Defaults as Def
import Options.Applicative

main :: IO ()
main = execCommand =<< customExecParser p opts
  where opts = info (commands <**> helper) fullDesc
        p = prefs showHelpOnError

data Options = Options
    { subCommand :: Command'
    , version :: Bool }
    deriving Show

data Command'
    = REPL [FilePath]
    | Eval String
    deriving Show


execCommand :: Options -> IO ()
execCommand (Options (REPL _) True) = putStrLn $ "iBlob v" <> Def.version
execCommand (Options (REPL fs) _) = customRunREPL replLoop (REPLOptions fs)
execCommand (Options (Eval c) _) = customRunREPL (replCheck $ Code c) (REPLOptions [])

commands :: Parser Options
commands = hsubparser
    (command "repl" (info replOption (progDesc "Run blob's REPL"))
    <> command "eval" (info evalOption (progDesc "Run some blob code directly")))

replOption :: Parser Options
replOption = Options <$> (REPL <$> many (strArgument (metavar "FILES..."))) <*> switch (long "version" <> short 'v' <> help "Print the version")

evalOption :: Parser Options
evalOption = Options <$> (Eval <$> strArgument (metavar "CODE")) <*> pure False