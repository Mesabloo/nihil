{-# LANGUAGE OverloadedStrings #-}

module Blob.REPL.Commands where

import Blob.Parsing.Types (Parser, Expr(..), Literal(..))
import Blob.REPL.Types (Command(..), EvalEnv, Value(..))
import Blob.Parsing.Lexer (string', space', space1', symbol)
import Blob.Parsing.Parser (statement, program)
import Blob.Parsing.ExprParser (expression)
import qualified Data.Map as Map
import Text.Megaparsec (try, hidden, eof, observing, (<|>), someTill, (<?>), anySingle, parseErrorTextPretty, choice, manyTill)
import Data.String.Utils (rstrip)
import Control.Monad.Reader (local, asks)
import Control.Monad.Except (throwError)
import Data.List (isInfixOf, intercalate)
import Data.Maybe (fromJust)
import Text.PrettyPrint.Leijen (text)
import System.Exit (exitSuccess)
import System.Console.ANSI (setSGR, SGR(..), ConsoleLayer(..), ColorIntensity(..), Color(..))
import Data.Functor (($>))


commands :: [String]
commands = [  ":help", ":h", ":?"
           ,  ":quit", ":q"
           ,  ":load", ":l" 
           ,  ":type", ":t"
           ,  ":kind", ":k"
           ,  ":eval", ":ev"
           , ":reset", ":r"
           ,   ":ast", ":a" ]

help :: Parser Command
help = space' *> try (hidden (string' "?" <|> string' "help" <|> string' "h") <* eof) $> Help <?> "߷"

exit :: Parser Command
exit = space' *> try (hidden (string' "quit" <|> string' "q") <* eof) $> Exit <?> "߷"

load :: Parser Command
load = do
    space' *> try (hidden (string' "load" <|> string' "l")) <?> "߷"

    end <- observing (try (space' *> eof))
    case end of
        Right _ -> fail "Missing argument “[file]”"
        Left _  -> do
            file <- observing $ space1' *> someTill anySingle eof
            case file of
                Left _  -> fail "Invalid argument “[file]”"
                Right x -> pure $ Load (rstrip x)

code :: Parser Command
code = (eof *> fail "") <|> Code <$> anySingle `someTill` eof

reset :: Parser Command
reset = space' *> try (hidden (string' "reset" <|> string' "r")) $> Reload <?> "߷"

getType :: Parser Command
getType = do
    space' *> try (hidden (string' "type" <|> string' "t")) <?> "߷"

    end <- observing . try $ space' *> eof
    case end of
        Right _ -> fail "Missing argument “[expr]”"
        Left _  -> GetType <$> (space1' *> anySingle `someTill` eof)

getKind :: Parser Command
getKind = do
    space' *> try (hidden (string' "kind" <|> string' "k")) <?> "߷"

    end <- observing . try $ space' *> eof
    case end of
        Right _ -> fail "Missing argument “[type]”"
        Left _  -> GetKind <$> (space1' *> anySingle `someTill` eof)

eval :: Parser Command
eval = do
    space' *> try (hidden (string' "eval" <|> string' "ev")) <?> "߷"

    end <- observing . try $ space' *> eof
    case end of
        Right _ -> fail "Missing argument “[expr]”"
        Left _  -> Eval <$> (space1' *> anySingle `someTill` eof)

ast :: Parser Command
ast = do
    space' *> try (hidden (string' "ast" <|> string' "a")) <?> "߷"

    end <- observing . try $ space' *> eof
    case end of
        Right _ -> fail "Missing argument “[code]”"
        Left _  -> Ast <$> (space1' *> anySingle `someTill` eof)

command :: Parser Command
command = do {
        space' *> symbol ":" ;
        cmd <- observing . try $ choice [help, exit, load, getType, getKind, eval, reset, ast] ;
        case cmd of
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
    pure $ "Unknown command `" ++ cmd ++ "`\n" ++ maybeYouWanted (':':cmd) commands

helpCommand :: IO ()
helpCommand = do
    setSGR [SetColor Foreground Vivid Magenta] >> putStr "“:help” ”:h” “:?”" >> setSGR [Reset]
        >> setSGR [SetColor Foreground Dull White] >> putStrLn ": show this menu." >> setSGR [Reset]

    setSGR [SetColor Foreground Vivid Magenta] >> putStr "“:quit” “:q”" >> setSGR [Reset]
        >> setSGR [SetColor Foreground Dull White] >> putStrLn ": exit the REPL." >> setSGR [Reset]

    setSGR [SetColor Foreground Vivid Magenta] >> putStr "“:load [file]” “:l [file]”" >> setSGR [Reset]
        >> setSGR [SetColor Foreground Dull White] >> putStrLn ": load a file into the REPL for further use." >> setSGR [Reset]

    setSGR [SetColor Foreground Vivid Magenta] >> putStr "“:type [expr]” “:t [expr]”" >> setSGR [Reset]
        >> setSGR [SetColor Foreground Dull White] >> putStrLn ": get the type of an expression." >> setSGR [Reset]

    setSGR [SetColor Foreground Vivid Magenta] >> putStr "“:kind [type]” “:k [type]”" >> setSGR [Reset]
        >> setSGR [SetColor Foreground Dull White] >> putStrLn ": get the kind of a type." >> setSGR [Reset]

    setSGR [SetColor Foreground Vivid Magenta] >> putStr "“:eval [expr]” “:ev [expr]”" >> setSGR [Reset]
        >> setSGR [SetColor Foreground Dull White] >> putStrLn ": evaluate an expression." >> setSGR [Reset]

    setSGR [SetColor Foreground Vivid Magenta] >> putStr "“:reset” “:r”" >> setSGR [Reset]
        >> setSGR [SetColor Foreground Dull White] >> putStrLn ": reset the REPL to its original state." >> setSGR [Reset]

    setSGR [SetColor Foreground Vivid Magenta] >> putStr "“:ast” “:a”" >> setSGR [Reset]
        >> setSGR [SetColor Foreground Dull White] >> putStrLn ": show the AST representation of some code." >> setSGR [Reset]

    setSGR [SetColor Foreground Dull White] >> putStrLn "\nYou also can write some core directly inside the REPL." >> setSGR [Reset]

exitCommand :: IO ()
exitCommand =
    setSGR [SetColor Foreground Vivid Green] >> putStrLn "See you soon!" >> setSGR [Reset]
        >> exitSuccess

evaluate :: Expr -> EvalEnv Value
evaluate (ELit (LInt v)) = pure $ VInt v
evaluate (ELit (LStr v)) = pure $ VStr v
evaluate (ELit (LDec v)) = pure $ VDec v
evaluate (EId id')       = fromJust <$> asks (Map.lookup id')
evaluate (ETuple es)     = VTuple <$> mapM evaluate es
evaluate (ELam x e)      = asks $ VLam x e
evaluate (EApp f x)      = do
    x' <- evaluate x
    f' <- evaluate f
    case f' of
        VLam x e c -> local (Map.insert x x' . Map.union c) (evaluate e)
        HLam f''   -> f'' x'
        v          -> throwError . text $ "Developer error: type checking failed ; expecting `VLam`, got `" ++ show v ++ "`.\nPlease report the issue."
evaluate (EList es)      = VList <$> mapM evaluate es



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
                                    then "Perhaps you meant " ++ s ++ "?"
                                    else ""
