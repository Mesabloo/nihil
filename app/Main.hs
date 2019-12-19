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

module Main
(
    main
) where

import Nihil.Interactive (customRunREPL, replLoop, replCheck)
import Nihil.Interactive.REPL (REPLOptions(..))
import Nihil.Interactive.Command (Command(Code))
import qualified Nihil.Interactive.Defaults as Def
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
    | Eval (Maybe String)
    deriving Show


execCommand :: Options -> IO ()
execCommand (Options (REPL _) True) = putStrLn $ "Nihili v" <> Def.version
execCommand (Options (REPL fs) _) = customRunREPL replLoop (REPLOptions fs)
execCommand (Options (Eval (Just c)) _) = customRunREPL (replCheck $ Code c) (REPLOptions [])
execCommand (Options (Eval Nothing) _) = do
    code <- getContents
    customRunREPL (replCheck $ Code code) (REPLOptions [])

commands :: Parser Options
commands = hsubparser
    (command "repl" (info replOption (progDesc "Run Nihil's REPL."))
    <> command "eval" (info evalOption (progDesc "Run some Nihil code directly. If no code is given, reads from stdin.")))

replOption :: Parser Options
replOption = Options <$> (REPL <$> many (strArgument (metavar "FILES..."))) <*> switch (long "version" <> short 'v' <> help "Print the version")

evalOption :: Parser Options
evalOption = Options <$> (Eval <$> optional (strArgument (metavar "CODE"))) <*> pure False