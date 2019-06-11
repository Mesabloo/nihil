{-# LANGUAGE OverloadedStrings, LambdaCase #-}

module Blob.REPL.Commands where

import Blob.Parsing.Types (Parser, Expr(..), Literal(..), Pattern(..))
import Blob.REPL.Types (Command(..), EvalEnv(..), Value(..), Scope(..), EvalState(..))
import Blob.Parsing.Lexer (string', space', space1', symbol, integer, keyword, lexemeN)
import Blob.Parsing.Parser (statement, program)
import Blob.Parsing.ExprParser (expression)
import qualified Data.Map as Map
import Text.Megaparsec (try, hidden, eof, observing, (<|>), someTill, (<?>), anySingle, parseErrorTextPretty, choice, manyTill, lookAhead)
import Data.String.Utils (rstrip)
import Control.Monad.Reader (local, asks, ask)
import Control.Monad.Except (throwError)
import Data.List (isInfixOf, intercalate)
import Data.Maybe (fromJust, isJust)
import Text.PrettyPrint.Leijen (text, Doc, dot, linebreak)
import System.Exit (exitSuccess)
import System.Console.ANSI (setSGR, SGR(..), ConsoleLayer(..), ColorIntensity(..), Color(..), ConsoleIntensity(..))
import Data.Functor (($>))
import Data.Char (isUpper)
import Control.Monad (join, zipWithM)
import Control.Applicative (empty)
import Data.List.Extra
import Debug.Trace


commands :: [String]
commands = [  ":help", ":h", ":?"
           ,  ":quit", ":q"
           ,  ":load", ":l" 
           ,  ":type", ":t"
           ,  ":kind", ":k"
           ,  ":eval", ":ev"
           , ":reset", ":r"
           ,   ":ast"
           ,  ":time"
           , ":bench"
           ,   ":env" ]

help :: Parser Command
help = space' *> (try . hidden . lexemeN) (keyword "?" <|> keyword "help" <|> keyword "h") $> Help <?> "߷"

exit :: Parser Command
exit = space' *> (try . hidden . lexemeN) (keyword "quit" <|> keyword "q") $> Exit <?> "߷"

load :: Parser Command
load = do
    space' *> (try . hidden . lexemeN) (keyword "load" <|> keyword "l") <?> "߷"

    end <- observing . lookAhead $ eof
    case end of
        Right _ -> fail "Missing argument “[file]”"
        Left _  -> do
            file <- anySingle `someTill` eof
            pure . Load $ rstrip file

code :: Parser Command
code = (eof *> fail "") <|> Code <$> anySingle `someTill` eof

reset :: Parser Command
reset = space' *> (try . hidden . lexemeN) (keyword "reset" <|> keyword "r") $> Reload <?> "߷"

getType :: Parser Command
getType = do
    space' *> (try . hidden . lexemeN) (keyword "type" <|> keyword "t") <?> "߷"

    end <- observing . lookAhead $  eof
    case end of
        Right _ -> fail "Missing argument “[expr]”"
        Left _  -> GetType <$> (anySingle `someTill` eof)

getKind :: Parser Command
getKind = do
    space' *> (try . hidden . lexemeN) (keyword "kind" <|> keyword "k") <?> "߷"

    end <- observing . lookAhead $ eof
    case end of
        Right _ -> fail "Missing argument “[type]”"
        Left _  -> GetKind <$> (anySingle `someTill` eof)

ast :: Parser Command
ast = do
    space' *> (try . hidden . lexemeN) (keyword "ast") <?> "߷"

    end <- observing . lookAhead $ eof
    case end of
        Right _ -> fail "Missing argument “[code]”"
        Left _  -> Ast <$> (anySingle `someTill` eof)

time :: Parser Command
time = do
    space' *> (try . hidden . lexemeN) (keyword "time") <?> "߷"

    end <- observing . lookAhead $ eof
    case end of
        Right _ -> fail "Missing argument “[expr]”"
        Left _  -> Time <$> (anySingle `someTill` eof)

bench :: Parser Command
bench = do
    space' *> (try . hidden . lexemeN) (keyword "bench") <?> "߷"

    end <- observing . lookAhead $ eof
    case end of
        Right _ -> fail "Missing arguments “[n] [expr]”"
        Left _  -> do
            n    <- integer
            end' <- observing . lookAhead $ eof
            case end' of
                Right _ -> fail "Missing argument “[expr]”"
                Left _  -> Bench n <$> (anySingle `someTill` eof)

env :: Parser Command
env = space' *> (try . hidden . lexemeN) (keyword "env") $> Env <?> "߷"

command :: Parser Command
command = do { space' *> symbol ":"
             ; cmd <- observing . try $ choice [help, exit, load, time, getType, getKind, reset, ast, bench, env] <* eof
             ; case cmd of
                Left err ->
                    if "߷" `isInfixOf` parseErrorTextPretty err
                    then do
                        msg <- makeCommandError
                        fail msg
                    else fail $ parseErrorTextPretty err
                Right x  -> pure x
             } <|> code

makeCommandError :: Parser String
makeCommandError = do
    cmd <- space' *> manyTill anySingle (space1' <|> eof)
    pure $ "Unknown command `" <> cmd <> "`\n" <> maybeYouWanted (':':cmd) commands

helpCommand :: IO ()
helpCommand = do
    setSGR [SetColor Foreground Vivid Magenta, SetConsoleIntensity BoldIntensity] >> putStr "“:help” ”:h” “:?”" >> setSGR [Reset]
        >> setSGR [SetColor Foreground Dull White] >> putStrLn ": show this menu." >> setSGR [Reset]

    setSGR [SetColor Foreground Vivid Magenta, SetConsoleIntensity BoldIntensity] >> putStr "“:quit” “:q”" >> setSGR [Reset]
        >> setSGR [SetColor Foreground Dull White] >> putStrLn ": exit the REPL." >> setSGR [Reset]

    setSGR [SetColor Foreground Vivid Magenta, SetConsoleIntensity BoldIntensity] >> putStr "“:load [file]” “:l [file]”" >> setSGR [Reset]
        >> setSGR [SetColor Foreground Dull White] >> putStrLn ": load a file into the REPL for further use." >> setSGR [Reset]

    setSGR [SetColor Foreground Vivid Magenta, SetConsoleIntensity BoldIntensity] >> putStr "“:type [expr]” “:t [expr]”" >> setSGR [Reset]
        >> setSGR [SetColor Foreground Dull White] >> putStrLn ": get the type of an expression." >> setSGR [Reset]

    setSGR [SetColor Foreground Vivid Magenta, SetConsoleIntensity BoldIntensity] >> putStr "“:kind [type]” “:k [type]”" >> setSGR [Reset]
        >> setSGR [SetColor Foreground Dull White] >> putStrLn ": get the kind of a type." >> setSGR [Reset]

    setSGR [SetColor Foreground Vivid Magenta, SetConsoleIntensity BoldIntensity] >> putStr "“:reset” “:r”" >> setSGR [Reset]
        >> setSGR [SetColor Foreground Dull White] >> putStrLn ": reset the REPL to its original state." >> setSGR [Reset]

    setSGR [SetColor Foreground Vivid Magenta, SetConsoleIntensity BoldIntensity] >> putStr "“:ast”" >> setSGR [Reset]
        >> setSGR [SetColor Foreground Dull White] >> putStrLn ": show the AST representation of some code." >> setSGR [Reset]

    setSGR [SetColor Foreground Vivid Magenta, SetConsoleIntensity BoldIntensity] >> putStr "“:time [expr]”" >> setSGR [Reset]
        >> setSGR [SetColor Foreground Dull White] >> putStrLn ": print the execution time of an expression." >> setSGR [Reset]

    setSGR [SetColor Foreground Vivid Magenta, SetConsoleIntensity BoldIntensity] >> putStr "“:bench [n] [expr]”" >> setSGR [Reset]
        >> setSGR [SetColor Foreground Dull White] >> putStrLn ": make some benchmark on an expression." >> setSGR [Reset]

    setSGR [SetColor Foreground Dull White] >> putStrLn "\nYou also can write some core directly inside the REPL." >> setSGR [Reset]

exitCommand :: IO ()
exitCommand =
    setSGR [SetColor Foreground Vivid Green] >> putStrLn "See you soon!" >> setSGR [Reset]
        >> exitSuccess

evaluate :: Expr -> EvalEnv Value
evaluate (ELit (LInt v))    = pure $ VInt v
evaluate (ELit (LStr v))    = pure $ VStr v
evaluate (ELit (LDec v))    = pure $ VDec v
evaluate (ELit (LChr v))    = pure $ VChr v
evaluate (EId id')          = do
                                    isNotCtor <- isJust . Map.lookup id' <$> asks vals

                                    if isNotCtor
                                    then fromJust . Map.lookup id' <$> asks vals
                                    else pure $ VCon id' []
evaluate (ETuple es)        = VTuple <$> mapM evaluate es
evaluate (ELam x e)         = VLam x e <$> asks vals
evaluate (EApp f x)         = do
    x' <- evaluate x
    f' <- evaluate f
    case f' of
        VLam x e c -> local (\env -> env { vals = Map.insert x x' . Map.union c $ vals env }) (evaluate e)
        HLam f''   -> f'' x'
        VCon id' e -> pure $ VCon id' (snoc e x')
        v          -> throwError . text $ "Developer error: type checking failed ; expecting `VLam`, `HLam` or `VCon` ; got `" <> show v <> "`.\nPlease report the issue."
-- evaluate (EList es)         = VList <$> mapM evaluate es
evaluate (EMatch expr pats) = join $ foldr ((<|>) . uncurry evalBranch) (pure makeMatchError) pats
  where evalBranch pat branch = do
            e <- evaluate expr
            s <- unpackPattern e pat
            pure (local (\env -> env { vals = s <> vals env }) (evaluate branch))

        unpackPattern :: Value -> Pattern -> EvalEnv (Scope Value)
        unpackPattern = curry $ \case
            (_, Wildcard)               -> pure mempty
            (VInt n, PInt n') | n == n' -> pure mempty
            (VStr s, PStr s') | s == s' -> pure mempty
            (VDec d, PDec d') | d == d' -> pure mempty
            (v, PId id')                -> pure $ Map.singleton id' v
            (VCon id' v, PCtor id'' v')
                | id' == id''           -> mconcat <$> zipWithM unpackPattern v v'
            _                           -> empty

        makeMatchError :: EvalEnv Value
        makeMatchError = throwError $ text "Non-exhaustive patterns in pattern matching" <> dot


levenshtein :: String -> String -> Int
levenshtein s1 s2
    | length s1 > length s2 = levenshtein s2 s1
    | length s1 < length s2 = let d = length s2 - length s1
                              in  d + levenshtein s1 (take (length s2 - d) s2)
levenshtein "" ""           = 0
levenshtein s1 s2
    | last s1 == last s2    = levenshtein (init s1) (init s2)
    | otherwise             = minimum [ 1 + levenshtein (init s1) s2
                                      , 1 + levenshtein s1 (init s2)
                                      , 1 + levenshtein (init s1) (init s2) ]

maybeYouWanted :: String -> [String] -> String
maybeYouWanted source choices = let s = intercalate ", " $
                                        filter (/= "")
                                        (map
                                            (\item -> if levenshtein source item > 3
                                                then ""
                                                else intercalate item ["“", "”"]
                                            )
                                            choices
                                        )
                                in
                                    if s /= ""
                                    then "Perhaps you meant " <> s <> "?"
                                    else ""
