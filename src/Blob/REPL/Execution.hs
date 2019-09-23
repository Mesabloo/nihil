{-# LANGUAGE LambdaCase #-}

module Blob.REPL.Execution where

import Blob.REPL.Types
import System.Console.ANSI
import System.Exit
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Blob.Language.Desugaring.Types hiding (Scheme(..))
import Blob.Language.Parsing.Annotation
import qualified Data.Map as Map
import Blob.Language.TypeChecking.Inference
import System.Directory
import Control.Monad
import Control.Monad.State
import Control.Monad.Except
import Text.PrettyPrint.Leijen hiding ((<$>))
import Blob.Language.Desugaring.Desugarer
import Blob.Language.Parsing.Parser (program, expression, type', runParser, runParser')
import Text.Megaparsec (eof, try)
import Control.Applicative
import Text.Megaparsec.Error (errorBundlePretty, ParseErrorBundle(..), parseErrorTextPretty, bundleErrors, ShowErrorComponent(..))
import Text.Megaparsec.Stream (Stream(..))
import Control.Monad.Reader
import Blob.Interpreter.Eval
import Blob.Language.TypeChecking.Types
import System.IO
import Blob.Language.Pretty.Parser hiding (pType)
import Blob.Language.Pretty.Inference
import Blob.Language.Lexing.Lexer
import Blob.Language.KindChecking.Checker
import Criterion.Measurement (secs, getTime)
import Blob.Interpreter.Types
import Blob.Prelude

helpCommand :: IO ()
helpCommand = do
    setSGR [SetColor Foreground Vivid Magenta, SetConsoleIntensity BoldIntensity] >> putStr "\":help\" \":h\" \":?\"" >> setSGR [Reset]
        >> putStrLn ": show this menu." >> setSGR [Reset]

    setSGR [SetColor Foreground Vivid Magenta, SetConsoleIntensity BoldIntensity] >> putStr "\":quit\" \":q\"" >> setSGR [Reset]
        >> putStrLn ": exit the REPL." >> setSGR [Reset]

    setSGR [SetColor Foreground Vivid Magenta, SetConsoleIntensity BoldIntensity] >> putStr "\":load [file]\" \":l [file]\"" >> setSGR [Reset]
        >> putStrLn ": load a file into the REPL for further use." >> setSGR [Reset]

    setSGR [SetColor Foreground Vivid Magenta, SetConsoleIntensity BoldIntensity] >> putStr "\":type [expr]\" \":t [expr]\"" >> setSGR [Reset]
        >> putStrLn ": get the type of an expression." >> setSGR [Reset]

    setSGR [SetColor Foreground Vivid Magenta, SetConsoleIntensity BoldIntensity] >> putStr "\":kind [type]\" \":k [type]\"" >> setSGR [Reset]
        >> putStrLn ": get the kind of a type." >> setSGR [Reset]

    setSGR [SetColor Foreground Vivid Magenta, SetConsoleIntensity BoldIntensity] >> putStr "\":reset {symbols}\" \":r {symbols}\"" >> setSGR [Reset]
        >> putStrLn ": reset the REPL to its original state or delete some user-defined symbols." >> setSGR [Reset]

    setSGR [SetColor Foreground Vivid Magenta, SetConsoleIntensity BoldIntensity] >> putStr "\":time [expr]\"" >> setSGR [Reset]
        >> putStrLn ": print the execution time of an expression." >> setSGR [Reset]

    setSGR [SetColor Foreground Vivid Magenta, SetConsoleIntensity BoldIntensity] >> putStr "\":bench [n] [expr]\"" >> setSGR [Reset]
        >> putStrLn ": make some benchmark on an expression." >> setSGR [Reset]

    setSGR [SetColor Foreground Vivid Magenta, SetConsoleIntensity BoldIntensity] >> putStr "\":env\"" >> setSGR [Reset]
        >> putStrLn ": print the whole current environment" >> setSGR [Reset]

    putStrLn "\nYou also can write some core directly inside the REPL." >> setSGR [Reset]

exitCommand :: IO ()
exitCommand =
    setSGR [SetColor Foreground Vivid Green] >> putStrLn "See you soon!" >> setSGR [Reset]
        >> exitSuccess

loadFile :: String -> REPL ()
loadFile file = do
    fileExists <- liftIO     $ doesFileExist file

    if not fileExists
    then lift $ throwError (text "File \"" <> text file <> text "\" not found" <> dot <> linebreak)
    else do

        content <- liftIO $ Text.readFile file
        st''    <- lift get
        let res = runLexer content file
        case res of
            Left err -> lift $ throwError (text $ errorBundlePretty err)
            Right tks -> do
                let res' = runParser tks file

                case res' of
                    Left err        -> lift $ throwError (printParseError err)
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
    let res = runLexer (Text.pack expr) "interactive"

    case res of
        Left err -> lift $ throwError (text $ errorBundlePretty err)
        Right tks -> do
            let res'           = runParser' (expression <* eof) tks "interactive"

            case res' of
                Left err  -> lift $ throwError (printParseError err)
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

    let res = runLexer (Text.pack typeExpr) "interactive"
    case res of
        Left err -> lift $ throwError (text $ errorBundlePretty err)
        Right tks -> do
            let res' = runParser' (type' <* eof) tks "interactive"

            case res' of
                Left err  -> lift $ throwError (printParseError err)
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
    let res = runLexer (Text.pack stat) "interactive"
    case res of
        Left err -> lift $ throwError (text $ errorBundlePretty err)
        Right x -> do
            let res' = runParser' ((Right <$> try (expression <* eof)) <|> (Left <$> (program <* eof))) x "interactive"

            case res' of
                Left err -> lift $ throwError (printParseError err)
                Right s ->
                    case s of
                        Right e -> do
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

                        Left p  -> do
                            let res1 = runSugar (runDesugarer "interactive" (p :- Nothing)) (op st)
                            case res1 of
                                Left err -> lift $ throwError err
                                Right (p'@(Program ss :- _), state') -> do
                                    lift . modify $ \st -> st { op = state' }

                                    case programTypeInference (ctx st) (tiProgram p') of
                                        Left err -> lift $ throwError err
                                        Right (_, state') -> do
                                            lift . modify $ \st' -> st' { ctx = state' }

                                            forM_ ss $ \case
                                                Definition id' expr :- _ -> do
                                                    st'   <- lift get
                                                    eval' <- liftIO . runExceptT $ runReaderT (evaluate expr) (values st')

                                                    case eval' of
                                                        Left err -> lift $ throwError err
                                                        Right evalRes -> lift . modify $ \st' -> st' { values = let env = values st'
                                                                                                                in env { vals = Map.insert id' evalRes (vals env) }}
                                                _                   -> pure ()

execTime :: String -> REPL ()
execTime expr = do
    st <- lift get
    let res = runLexer (Text.pack expr) "interactive"
    case res of
        Left err -> lift $ throwError (text $ errorBundlePretty err)
        Right tks -> do
            let e = runParser' (try expression <* eof) tks "interactive"
            case e of
                Left err     -> lift $ throwError (printParseError err)
                Right e -> do
                    let res1 = runSugar ( do { accumulateOnExpression e
                                                ; desugarExpression "interactive" e } ) (op st)
                    case res1 of
                        Left err -> lift $ throwError err
                        Right (e, _) -> do
                            let t   = inferExpr (ctx st) e
                            case t of
                                Left err -> lift $ throwError err
                                Right _ -> do
                                    (t, res) <- liftIO . time . runExceptT $ runReaderT (evaluate e) (values st)
                                    case res of
                                        Left err      -> lift $ throwError err
                                        Right evalRes ->
                                            liftIO $ setSGR [SetColor Foreground Vivid Cyan]
                                                >> print evalRes
                                                >> setSGR [Reset]
                                                >> hFlush stdout

                                    liftIO $ setSGR [SetColor Foreground Vivid Yellow]
                                        >> putStrLn ("Time taken: " <> secs t)
                                        >> setSGR [Reset]
                                        >> hFlush stdout

execBench :: Integer -> String -> REPL ()
execBench n expr = do
    st <- lift get

    let res = runLexer (Text.pack expr) "interactive"
    case res of
        Left err -> lift $ throwError (text $ errorBundlePretty err)
        Right tks -> do
            let e = runParser' (try expression <* eof) tks "interactive"
            case e of
                Left err     -> lift $ throwError (printParseError err)
                Right e -> do
                    let res1 = runSugar ( do { accumulateOnExpression e
                                                ; desugarExpression "interactive" e } ) (op st)
                    case res1 of
                        Left err -> lift $ throwError err
                        Right (e, _) -> do
                            let t   = inferExpr (ctx st) e
                            case t of
                                Left err -> lift $ throwError err
                                Right _ -> do
                                    t' <- liftIO . replicateM (fromIntegral n) . time $ runExceptT $ runReaderT (evaluate e) (values st)

                                    let t   = map (uncurry const) t'
                                        min = minimum t
                                        max = maximum t
                                        avg = uncurry (/) . foldr (\e (s, c) -> (e + s, c + 1)) (0.0, 0.0) $ t
                                        stddev =
                                            let sigma = sum $ map ((^ 2) . subtract avg) t
                                            in sqrt (sigma / fromIntegral n)

                                    liftIO $ do
                                        setSGR [SetColor Foreground Vivid Yellow]
                                        putStrLn $ "Results for " <> show n <> " runs:"
                                        putStrLn $ "> Minimum: " <> secs min
                                        putStrLn $ "> Maximum: " <> secs max
                                        putStrLn $ "> Average: " <> secs avg
                                        putStrLn $ "> Std dev: " <> secs stddev
                                        setSGR [Reset]
                                        hFlush stdout

getEnv :: REPL ()
getEnv = do
    st  <- lift (gets ctx)
    let types           = Map.toList $ typeDeclCtx st
        (TypeEnv funs)  = defCtx st <> ctorCtx st
        funs'           = Map.toList funs
        showKinds       =
            forM_ types $
                \(name, kind) -> do
                    putStr "\t"
                    setSGR [SetColor Foreground Vivid Yellow] *> putStr name
                    setSGR [Reset] *> putStr " :: "
                    setSGR [SetColor Foreground Vivid Cyan] *> print (pKind kind)
                    setSGR [Reset]
        showFuns        =
            forM_ funs' $
                \(name, Scheme _ type') -> do
                    putStr "\t"
                    setSGR [SetColor Foreground Vivid Yellow] *> putStr name
                    setSGR [Reset] *> putStr " :: "
                    setSGR [SetColor Foreground Vivid Cyan] *> print (pType (type' :- Nothing))
                    setSGR [Reset]

    liftIO $ putStrLn "Types:" *> showKinds *> putStrLn ""
    liftIO $ putStrLn "Functions:" *> showFuns

resetEnv :: [String] -> REPL () -> REPL ()
resetEnv [] reload = do
    lift . modify $ \st -> st { ctx = initGlobalEnv
                              , values = initEvalState }
    reload
resetEnv [x] _ = resetOne x
resetEnv (x:xs) r = resetOne x *> resetEnv xs r

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

printParseError :: (Stream s, ShowErrorComponent e) => ParseErrorBundle s e -> Doc
printParseError = text . concatMap parseErrorTextPretty . bundleErrors
