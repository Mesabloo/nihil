{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, LambdaCase, TupleSections, BangPatterns #-}

module Blob.REPL.REPL
( runREPL
, replLoop
, REPLError
) where

import System.Console.Haskeline (getInputLine, runInputT, InputT, MonadException(..), RunIO(..), defaultSettings, withInterrupt, handleInterrupt)
import Control.Monad.Except (runExceptT, throwError, catchError, MonadError, ExceptT(..), runExcept)
import Control.Monad.State (StateT(..), evalStateT, lift, get, MonadIO, liftIO, runState, modify, evalState, gets)
import Control.Monad.Reader (runReaderT)
import Blob.REPL.Types (REPLError, REPLState(..), REPL, Command(..), Value(..), EvalState(..))
import Blob.Inference.Types (GlobalEnv(..), TypeEnv(..), CustomScheme(..), Scheme(..))
import Blob.Parsing.Types (Program(..), Statement(..), ParseState(..))
import Blob.Parsing.Defaults (initParseState)
import Blob.REPL.Prelude (defaultEnv, initGlobalEnv, initEvalState)
import Blob.REPL.Commands (command, helpCommand, exitCommand, evaluate)
import Blob.Parsing.Parser (program, statement)
import Blob.Parsing.ExprParser (expression)
import Blob.Parsing.TypeParser (type')
import Blob.Inference.AlgorithmW (programTypeInference, tiProgram, tiType, tiCustomType, checkTI, typeInference)
import Blob.KindChecking.Checker (kiType, checkKI, kindInference)
import Blob.PrettyPrinter.PrettyParser (pExpression, pStatement)
import Blob.PrettyPrinter.PrettyInference (pType, pKind)
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Void (Void)
import qualified Data.Text as Text (Text, pack)
import qualified Data.Text.IO as Text (readFile)
import System.Console.ANSI (setSGR, SGR(..), ColorIntensity(..), Color(..), ConsoleLayer(..), ConsoleIntensity(..))
import Text.Megaparsec.Error (ParseErrorBundle(..), errorBundlePretty, bundleErrors, parseErrorTextPretty)
import Text.Megaparsec (runParser, PosState(..), (<|>), eof, try)
import Text.Megaparsec.Pos (SourcePos(..), unPos, mkPos)
import System.IO (hFlush, stdout)
import Control.Monad (forever, void, forM, replicateM)
import System.Directory (doesFileExist, getCurrentDirectory)
import Data.List.NonEmpty (toList)
import Data.List.Utils (split)
import Data.String.Utils (rstrip)
import Data.Char (toUpper)
import Criterion.Measurement (secs, initializeTime, getTime, measure, runBenchmark)
import Criterion.Measurement.Types (whnf, Measured(..))
import Debug.Trace
import System.IO.Unsafe (unsafePerformIO)
import Text.PrettyPrint.Leijen (text, (<$$>), empty)

runREPL :: REPL a -> IO ()
runREPL r = do
    initializeTime 
    liftIO $ do
        replSetColor Vivid White >> putStr "Blob v0.0.1\nType " >> setSGR [Reset]
        setSGR [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Magenta] >> putStr "“:?”" >> setSGR [Reset]
        replSetColor Vivid White >> putStrLn " for a list of commands." >> setSGR [Reset]
        hFlush stdout

    handleInterrupt exitCommand . void $ runExceptT (evalStateT (runInputT defaultSettings (withInterrupt r)) initREPLState)
  where initREPLState = REPLState { ctx = initGlobalEnv
                                  , op = initParseState
                                  , values = initEvalState
                                  
                                  , lastExecTime = 0.0 }

{-# NOINLINE replLoop #-}
replLoop :: REPL ()
replLoop = forever $ do

    time <- lift (gets lastExecTime)
    let str = if time /= 0.0
              then secs time <> " "
              else ""

    liftIO $ setSGR [SetColor Foreground Dull Blue]
    input <- getInputLine $ str <> "⮞ β ⮞ "
    liftIO $ setSGR [Reset]

    lift . modify $ \st -> st { lastExecTime = 0.0 }
    
    case input of
        Nothing     -> liftIO $ putStr ""
        Just input' -> do
            env <- lift get
            let res = runParser (runStateT command (op env)) "" (Text.pack input')

            case res of
                Left err               -> liftIO $ replSetColor Vivid Red >> mapM_ (putStr . parseErrorTextPretty) (toList $ bundleErrors err) >> setSGR [Reset] >> hFlush stdout
                Right (output, state') -> do
                    lift . modify $ \st -> st { op = state' }

                    replCheck output
    
replCheck :: Command -> REPL ()
replCheck = \case
    Help                -> liftIO helpCommand
    Exit                -> liftIO exitCommand
    Reload              ->
        lift . modify $ \st -> st { ctx = initGlobalEnv
                                    , op = initParseState
                                    , values = initEvalState }
    Load file           -> do
        fileExists <- liftIO     $ doesFileExist file
        if fileExists
        then do
            content <- liftIO $ Text.readFile file
            st''    <- lift get
            let res = runParser (runStateT program (op st'')) file content
            
            case res of
                Left err                           -> liftIO $ replSetColor Vivid Red >> putStr (errorBundlePretty err) >> setSGR [Reset] >> hFlush stdout
                Right (ast@(Program stmts), state) -> do
                    lift . modify $ \st -> st { op = ParseState { operators = operators state
                                                        , currentIndent = mkPos 1 } }

                    case programTypeInference (ctx st'') (tiProgram ast) of
                        Left err          -> liftIO $ replSetColor Vivid Red >> putStr (show err) >> setSGR [Reset] >> hFlush stdout
                        Right (_, state') -> do
                            lift . modify $ \st' -> st' { ctx = state' }

                            mapM_ (\case
                                Definition id' expr -> do
                                    st'' <- lift get
                                    eval' <- liftIO . runExceptT $ runReaderT (evaluate expr) (values st'')

                                    case eval' of
                                        Left err      -> liftIO $ replSetColor Vivid Red >> putStr (show err) >> setSGR [Reset] >> hFlush stdout
                                        Right evalRes -> lift . modify $ \st' -> st' { values = let env  = values st'
                                                                                                in env { vals = Map.insert id' evalRes (vals env) } }
                                _                   -> liftIO $ putStr ""
                                ) stmts
        else
            liftIO $ replSetColor Vivid Red >> putStrLn ("Unknown file `" <> file <> "`. Does it exist?") >> setSGR [Reset] >> hFlush stdout
    GetType expr        -> do
        st <- lift get
        let (TypeEnv env) = defCtx $ ctx st
            res           = runParser (evalStateT (expression <* eof) (op st)) "interactive" (Text.pack expr)

        case res of
            Left err  -> liftIO $ replSetColor Vivid Red >> putStr (errorBundlePretty err) >> setSGR [Reset] >> hFlush stdout
            Right e   -> do
                let t = runExcept (evalStateT (checkTI $ typeInference (TypeEnv env) e) (ctx st))
                case t of
                    Left err    -> liftIO $ replSetColor Vivid Red >> putStr (show err) >> setSGR [Reset] >> hFlush stdout
                    Right type' ->  liftIO $ replSetColor Vivid Yellow >> putStr (show (pExpression e (0 :: Int))) >> setSGR [Reset] >> putStr " :: " >> replSetColor Vivid Cyan >> print (pType type') >> setSGR [Reset] >> hFlush stdout

    GetKind typeExpr    -> do
        st <- lift get
        let env = typeDeclCtx $ ctx st
            res = tiType <$> runParser (evalStateT (type' <* eof) (op st)) "interactive" (Text.pack typeExpr)

        case res of
            Left err  -> liftIO $ replSetColor Vivid Red >> putStr (errorBundlePretty err) >> setSGR [Reset] >> hFlush stdout
            Right t   -> do
                let k = runExcept (evalStateT (checkKI $ kindInference env t) (ctx st))
                case k of
                    Left err   -> liftIO $ replSetColor Vivid Red >> putStr (show err) >> setSGR [Reset] >> hFlush stdout
                    Right kind -> liftIO $ replSetColor Vivid Yellow >> putStr (show (pType t)) >> setSGR [Reset] >> putStr " :: " >> replSetColor Vivid Cyan >> print (pKind kind) >> setSGR [Reset] >> hFlush stdout
        
    Code stat           -> do
        st <- lift get
        let res = runParser (runStateT (((Right <$> try (expression <* eof)) <|> (Left <$> statement)) <* eof) (op st)) "interactive" (Text.pack stat)
        
        case res of
            Left err -> liftIO $ replSetColor Vivid Red >> putStr (errorBundlePretty err) >> setSGR [Reset] >> hFlush stdout
            Right (s, state) -> do
                lift . modify $ \st -> st { op = ParseState { operators = operators state
                                                    , currentIndent = mkPos 1 } }

                case s of
                    Right e -> do
                        let env = defCtx $ ctx st
                            t   = runExcept (evalStateT (checkTI $ typeInference env e) (ctx st))
                        case t of
                            Left err -> liftIO $ replSetColor Vivid Red >> putStr (show err) >> setSGR [Reset] >> hFlush stdout
                            Right _  -> do
                                res <- liftIO . runExceptT $ runReaderT (evaluate e) (values st)
                                case res of
                                    Left err      -> liftIO $ replSetColor Vivid Red >> print err >> setSGR [Reset] >> hFlush stdout
                                    Right evalRes -> liftIO $ replSetColor Vivid Cyan >> print evalRes >> setSGR [Reset] >> hFlush stdout

                    Left s  -> case programTypeInference (ctx st) (tiProgram $ Program [s]) of
                        Left err -> liftIO $ replSetColor Vivid Red >> putStr (show err) >> setSGR [Reset] >> hFlush stdout
                        Right (_, state') -> do
                            lift . modify $ \st' -> st' { ctx = state' }

                            case s of
                                Definition id' expr -> do
                                    st'   <- lift get
                                    eval' <- liftIO . runExceptT $ runReaderT (evaluate expr) (values st')

                                    case eval' of
                                        Left err -> liftIO $ replSetColor Vivid Red >> putStr (show err) >> setSGR [Reset] >> hFlush stdout
                                        Right evalRes -> lift . modify $ \st' -> st' { values = let env = values st'
                                                                                                in env { vals = Map.insert id' evalRes (vals env) }}
                                _                   -> pure ()

                            liftIO $ replSetColor Dull Green >> putStr "" >> setSGR [Reset] >> hFlush stdout
    Ast ast            -> do
        st <- lift get
        let res = runParser (evalStateT (statement <* eof) (op st)) "interactive" (Text.pack ast)

        case res of
            Left err -> liftIO $ replSetColor Vivid Red >> putStr (errorBundlePretty err) >> setSGR [Reset] >> hFlush stdout
            Right s  -> liftIO . print $ pStatement s 0
    Time expr          -> do
        st <- lift get
        let e = runParser (runStateT (try expression <* eof) (op st)) "interactive" (Text.pack expr)
        case e of
            Left err     -> liftIO $ replSetColor Vivid Red >> putStr (errorBundlePretty err) >> setSGR [Reset] >> hFlush stdout
            Right (e, _) -> do
                let env = defCtx $ ctx st
                    t   = runExcept (evalStateT (checkTI $ typeInference env e) (ctx st))
                case t of
                    Left err -> liftIO $ replSetColor Vivid Red >> putStr (show err) >> setSGR [Reset] >> hFlush stdout
                    Right _  -> do
                        (t, res) <- liftIO . time $ runExceptT $ runReaderT (evaluate e) (values st)
                        liftIO $ case res of
                            Left err      -> replSetColor Vivid Red >> print err >> setSGR [Reset] >> hFlush stdout
                            Right evalRes -> replSetColor Vivid Cyan >> print evalRes >> setSGR [Reset] >> hFlush stdout

                        lift . modify $ \st -> st { lastExecTime = t }
    Bench n expr       -> do
        st <- lift get

        let e = runParser (runStateT (try expression <* eof) (op st)) "interactive" (Text.pack expr)
        case e of
            Left err     -> liftIO $ replSetColor Vivid Red >> putStr (errorBundlePretty err) >> setSGR [Reset] >> hFlush stdout
            Right (e, _) -> do
                let env = defCtx $ ctx st
                    t   = runExcept (evalStateT (checkTI $ typeInference env e) (ctx st))
                case t of
                    Left err -> liftIO $ replSetColor Vivid Red >> putStr (show err) >> setSGR [Reset] >> hFlush stdout
                    Right _  -> do
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
                        liftIO . putStrLn $ "-   Total: " <> secs tot
    Env                -> do
        st  <- lift (gets ctx)
        st' <- lift (gets values)
        let (t:types)                = Map.toList $ typeDeclCtx st
            (TypeEnv funs)           = defCtx st <> ctorCtx st
            ((id, Scheme _ f):funs') = Map.toList funs
            vals'                    = Map.toList (vals st')

            kinds                    = foldl
                                            (\acc (k, v) -> acc <$$> text "  " <> text k <> text " :: " <> pKind v)
                                            (text "  " <> text (fst t) <> text " :: " <> pKind (snd t))
                                            types

            functions                = foldl
                                            (\acc (k, Scheme _ v) -> acc <$$> text "  " <> text k <> text " :: " <> pType v)
                                            (text "  " <> text id <> text " :: " <> pType f)
                                            funs'

            values'                  = case vals' of
                                            []        -> empty
                                            (v:vals') -> foldl
                                                            (\acc (k, v') -> acc <$$> text "  " <> text k <> text " = " <> text (show v'))
                                                            (text "  " <> text (fst v) <> text " = " <> text (show (snd v)))
                                                            vals'

        liftIO . putStrLn $
            "Types:\n" <> show kinds <> "\n"
         <> "Functions:\n" <> show functions <> "\n"
         <> "Values:\n" <> show values'






time :: IO a -> IO (Double, a)
time f = do
    begin  <- getTime
    result <- f
    end    <- getTime
    pure $ (,) (end - begin) result

replSetColor :: MonadIO m => ColorIntensity -> Color -> m ()
replSetColor intensity color = liftIO $ setSGR [SetColor Foreground intensity color]

replErrorPretty :: ParseErrorBundle Text.Text Void -> IO ()
replErrorPretty bundle = do
    let (PosState _ _ state _ _) = bundlePosState bundle
        errors = map (, state) (toList $ bundleErrors bundle)
        texts  = flip List.map errors $ \(e, pos) -> do let (SourcePos name line col) = pos
                                                        "at <“" <> name <> "”:" <> show (unPos line) <> ":" <> show (unPos col) <> ">\n" <> parseErrorTextPretty e
        lines_ = List.map (split "\n") texts
        lines' = List.map (filter (/= "")) lines_
        errs   = List.map (List.map (capitalize . rstrip . flip (<>) ".")) lines'
        errs'  = List.map (List.intercalate "\n") errs
    mapM_ putStr errs' >> putStr "\n"


capitalize :: String -> String
capitalize "" = ""
capitalize (x:xs) = toUpper x : xs











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
