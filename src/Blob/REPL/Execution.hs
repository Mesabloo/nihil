{-# LANGUAGE LambdaCase #-}

module Blob.REPL.Execution where

import Blob.REPL.Types
import System.Console.ANSI
import System.Exit
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Blob.Desugaring.Types hiding (Scheme(..))
import Blob.Parsing.Annotation
import qualified Data.Map as Map
import Blob.TypeChecking.Inference
import Blob.Interpreter.Types
import System.Directory
import Control.Monad
import Control.Monad.State
import Control.Monad.Except
import Text.PrettyPrint.Leijen hiding ((<$>))
import Blob.Desugaring.Desugarer
import Blob.Parsing.Parser
import Control.Applicative
import Text.Megaparsec.Error (errorBundlePretty)
import Control.Monad.Reader
import Blob.Interpreter.Eval
import Blob.TypeChecking.Types
import System.IO
import Blob.Pretty.Parser hiding (pType)
import Blob.Pretty.Inference
import Blob.Parsing.ExprParser
import Blob.Parsing.Lexer
import Blob.Parsing.TypeParser
import Blob.KindChecking.Types
import Blob.KindChecking.Checker
import Text.Megaparsec (try)
import Criterion.Measurement (secs, getTime)
import qualified Data.List as List
import Blob.Interpreter.Types
import Blob.Prelude

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

    setSGR [SetColor Foreground Vivid Magenta, SetConsoleIntensity BoldIntensity] >> putStr "“:reset {symbols}” “:r {symbols}”" >> setSGR [Reset]
        >> setSGR [SetColor Foreground Dull White] >> putStrLn ": reset the REPL to its original state or delete some user-defined symbols." >> setSGR [Reset]

    setSGR [SetColor Foreground Vivid Magenta, SetConsoleIntensity BoldIntensity] >> putStr "“:time [expr]”" >> setSGR [Reset]
        >> setSGR [SetColor Foreground Dull White] >> putStrLn ": print the execution time of an expression." >> setSGR [Reset]

    setSGR [SetColor Foreground Vivid Magenta, SetConsoleIntensity BoldIntensity] >> putStr "“:bench [n] [expr]”" >> setSGR [Reset]
        >> setSGR [SetColor Foreground Dull White] >> putStrLn ": make some benchmark on an expression." >> setSGR [Reset]

    setSGR [SetColor Foreground Dull White] >> putStrLn "\nYou also can write some core directly inside the REPL." >> setSGR [Reset]

exitCommand :: IO ()
exitCommand =
    setSGR [SetColor Foreground Vivid Green] >> putStrLn "See you soon!" >> setSGR [Reset]
        >> exitSuccess

loadFile :: String -> REPL ()
loadFile file = do
    fileExists <- liftIO     $ doesFileExist file

    if not fileExists
    then lift $ throwError (text "File “" <> text file <> text "” not found" <> dot <> linebreak)
    else do

        content <- liftIO $ Text.readFile file
        st''    <- lift get
        let res = runParser parseProgram file content
        
        case res of
            Left err        -> lift $ throwError (text $ errorBundlePretty err)
            Right parseTree -> do
                let state' = op st''
                let res1 = runSugar (runDesugarer file (parseTree :- Nothing)) state'

                case res1 of
                    Left err            -> lift $ throwError err
                    Right (ast, state') -> do
                        lift . modify $ \ st -> st { op = state' }

                        case programTypeInference (ctx st'') (tiProgram ast) of
                            Left err          -> lift $ throwError err
                            Right (_, state') -> do
                                lift . modify $ \st' -> st' { ctx = state' }

                                let (Program stmts) = getAnnotated ast

                                mapM_ (\case
                                    Definition id' expr :- _ -> do
                                        st'' <- lift get
                                        eval' <- liftIO . runExceptT $ runReaderT (evaluate expr) (values st'')

                                        case eval' of
                                            Left err      -> lift $ throwError err
                                            Right evalRes -> lift . modify $ \st' -> st' { values = let env  = values st'
                                                                                                    in env { vals = Map.insert id' evalRes (vals env) } }
                                    _                   -> pure ()
                                    ) stmts

getType :: String -> REPL ()
getType expr = do
    st <- lift get
    let (TypeEnv env) = defCtx $ ctx st
        res           = runParser (parseExpression <* eof) "interactive" (Text.pack expr)

    case res of
        Left err  -> lift $ throwError (text $ errorBundlePretty err)
        Right e   -> do
            let res1 = runSugar (do { accumulateOnExpression e
                                    ; desugarExpression "interactive" e } ) (op st)
            case res1 of
                Left err -> lift $ throwError err
                Right (e, _) -> do
                    let t = inferExpr (ctx st) e
                    case t of
                        Left err -> lift $ throwError err
                        Right (Scheme _ type') ->
                            liftIO $ setSGR [SetColor Foreground Vivid Yellow]
                                     >> putStr (show (pExpression e))
                                     >> setSGR [Reset]
                                     >> putStr " :: "
                                     >> setSGR [SetColor Foreground Vivid Cyan]
                                     >> print (pType (type' :- Nothing))
                                     >> setSGR [Reset]
                                     >> hFlush stdout

getKind :: String -> REPL ()
getKind typeExpr = do
    st <- lift get
    let env = typeDeclCtx $ ctx st
        res = runParser (type' <* eof) "interactive" (Text.pack typeExpr)

    case res of
        Left err  -> lift $ throwError (text $ errorBundlePretty err)
        Right t   -> do
            let res1 = runSugar (desugarType "interactive" t) (op st)
            case res1 of
                Left err -> lift $ throwError err
                Right (t, _) -> do
                    let t1 = tiType t
                    let k = runExcept (evalStateT (checkKI $ kindInference env t1) (ctx st))
                    case k of
                        Left err   -> lift $ throwError err
                        Right kind ->
                            liftIO $ setSGR [SetColor Foreground Vivid Yellow]
                                     >> putStr (show (pType (t1 :- Nothing)))
                                     >> setSGR [Reset] 
                                     >> putStr " :: " 
                                     >> setSGR [SetColor Foreground Vivid Cyan]
                                     >> print (pKind kind) 
                                     >> setSGR [Reset] 
                                     >> hFlush stdout

execCode :: String -> REPL ()
execCode stat = do
    st <- lift get
    let res = runParser (((Right <$> try (parseExpression <* eof)) <|> (Left <$> parseStatement)) <* eof) "interactive" (Text.pack stat)
    
    case res of
        Left err -> lift $ throwError (text $ errorBundlePretty err)
        Right s ->
            case s of
                Right e -> do
                    let env = defCtx $ ctx st

                    let res1 = runSugar ( do { accumulateOnExpression e
                                                ; desugarExpression "interactive" e } ) (op st)
                    case res1 of
                        Left err -> lift $ throwError err
                        Right (e, _) -> do
                            let t   = inferExpr (ctx st) e
                            case t of
                                Left err -> lift $ throwError err
                                Right _ -> do
                                    res <- liftIO . runExceptT $ runReaderT (evaluate e) (values st)
                                    case res of
                                        Left err      -> lift $ throwError err
                                        Right evalRes -> liftIO $ setSGR [SetColor Foreground Vivid Cyan]
                                                                  >> print evalRes
                                                                  >> setSGR [Reset]
                                                                  >> hFlush stdout

                Left s  -> do
                    let res1 = runSugar (runDesugarer "interactive" ([s] :- Nothing)) (op st)
                    case res1 of
                        Left err -> lift $ throwError err
                        Right (s'@(Program [s] :- _), state') -> do
                            lift . modify $ \st -> st { op = state' }

                            case programTypeInference (ctx st) (tiProgram s') of
                                Left err -> lift $ throwError err
                                Right (_, state') -> do
                                    lift . modify $ \st' -> st' { ctx = state' }

                                    case s of
                                        Definition id' expr :- _ -> do
                                            st'   <- lift get
                                            eval' <- liftIO . runExceptT $ runReaderT (evaluate expr) (values st')

                                            case eval' of
                                                Left err -> lift $ throwError err
                                                Right evalRes -> lift . modify $ \st' -> st' { values = let env = values st'
                                                                                                        in env { vals = Map.insert id' evalRes (vals env) }}
                                        _                   -> pure ()
                        _ -> pure ()

execTime :: String -> REPL ()
execTime expr = do
    st <- lift get
    let e = runParser (try parseExpression <* eof) "interactive" (Text.pack expr)
    case e of
        Left err     -> lift $ throwError (text $ errorBundlePretty err)
        Right e -> do
            let res1 = runSugar ( do { accumulateOnExpression e
                                        ; desugarExpression "interactive" e } ) (op st)
            case res1 of
                Left err -> lift $ throwError err
                Right (e, _) -> do
                    let env = defCtx $ ctx st
                        t   = inferExpr (ctx st) e
                    case t of
                        Left err -> lift $ throwError err
                        Right _ -> do
                            (t, res) <- liftIO . time . runExceptT $ runReaderT (evaluate e) (values st)
                            case res of
                                Left err      -> lift $ throwError err
                                Right evalRes -> liftIO $ setSGR [SetColor Foreground Vivid Cyan]
                                                          >> print evalRes
                                                          >> setSGR [Reset]
                                                          >> hFlush stdout

                            lift . modify $ \st -> st { lastExecTime = t }

execBench :: Integer -> String -> REPL ()
execBench n expr = do
    st <- lift get

    let e = runParser (try parseExpression <* eof) "interactive" (Text.pack expr)
    case e of
        Left err     -> lift $ throwError (text $ errorBundlePretty err) 
        Right e -> do
            let res1 = runSugar ( do { accumulateOnExpression e
                                        ; desugarExpression "interactive" e } ) (op st)
            case res1 of
                Left err -> lift $ throwError err
                Right (e, _) -> do
                    let env = defCtx $ ctx st
                        t   = inferExpr (ctx st) e
                    case t of
                        Left err -> lift $ throwError err
                        Right _ -> do
                            t' <- liftIO . replicateM (fromIntegral n) . time $ runExceptT $ runReaderT (evaluate e) (values st)

                            let t   = map (uncurry const) t'
                                min = minimum t
                                max = maximum t
                                avg = uncurry (/) . foldr (\e (s, c) -> (e + s, c + 1)) (0.0, 0.0) $ t
                                tot = List.foldl' (+) 0.0 t

                            liftIO . putStrLn $ "Results for " <> show n <> " runs:"
                            liftIO . putStrLn $ "- Minimum: " <> secs min
                            liftIO . putStrLn $ "- Maximum: " <> secs max
                            liftIO . putStrLn $ "- Average: " <> secs avg

                            lift . modify $ \st -> st { lastExecTime = tot }

getEnv :: REPL ()
getEnv = do
    st  <- lift (gets ctx)
    let (t:types)                = Map.toList $ typeDeclCtx st
        (TypeEnv funs)           = defCtx st <> ctorCtx st
        ((id, Scheme _ f):funs') = Map.toList funs

        kinds                    = foldl
                                        (\acc (k, v) -> acc <$$> text "  " <> text k <> text " :: " <> pKind v)
                                        (text "  " <> text (fst t) <> text " :: " <> pKind (snd t))
                                        types

        functions                = foldl
                                        (\acc (k, Scheme _ v) -> acc <$$> text "  " <> text k <> text " :: " <> pType (v :- Nothing))
                                        (text "  " <> text id <> text " :: " <> pType (f :- Nothing))
                                        funs'

    liftIO . putStrLn $
        "Types:\n" <> show kinds <> "\n"
        <> "Functions:\n" <> show functions

resetEnv :: [String] -> REPL ()
resetEnv [] = lift . modify $ \st -> st { ctx = initGlobalEnv
                                        , values = initEvalState }
resetEnv [x] = resetOne x
resetEnv (x:xs) = resetOne x *> resetEnv xs

resetOne :: String -> REPL ()
resetOne x = lift . modify $
    \st -> st { ctx = let env = ctx st
                          newDefs = Map.delete x (getMap $ defCtx env)
                      in GlobalEnv (typeDeclCtx env) (typeDefCtx env) (TypeEnv newDefs) (ctorCtx env)
              , values = let env = values st
                             newEvals = Map.delete x (vals env)
                         in EvalState newEvals (ctors env) }




time :: IO a -> IO (Double, a)
time f = do
    begin  <- getTime
    result <- f
    end    <- getTime
    pure $ (,) (end - begin) result