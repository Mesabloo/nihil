module Main 
(
    main
) where

import Blob.REPL.REPL
import qualified Blob.REPL.Defaults as Def
import Options.Applicative

main :: IO ()
main = execCommand =<< customExecParser p opts
  where opts = info (commands <**> helper) fullDesc
        p = prefs showHelpOnError

data Options = Options
    { subCommand :: Command
    , version :: Bool }
    deriving Show

data Command =
    REPL [FilePath]
    deriving Show


execCommand :: Options -> IO ()
execCommand (Options (REPL _) True) = putStrLn $ "iBlob v" <> Def.version
execCommand (Options (REPL fs) _) = customRunREPL replLoop (REPLOptions fs)

commands :: Parser Options
commands = hsubparser
    ( command "repl" (info replOption ( progDesc "Run blob's REPL" )) )

replOption :: Parser Options
replOption = Options <$> (REPL <$> many (strArgument (metavar "FILES..."))) <*> switch ( long "version" <> short 'v' <> help "Print the version" )