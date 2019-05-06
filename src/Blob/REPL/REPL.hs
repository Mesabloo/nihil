{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, LambdaCase, TupleSections #-}

module Blob.REPL.REPL
( runREPL
, replLoop
, REPLError
) where

import System.Console.Haskeline (getInputLine, runInputT, InputT, MonadException(..), RunIO(..), defaultSettings)
import Control.Monad.Except (runExceptT, throwError, catchError, MonadError, ExceptT(..), runExcept)
import Control.Monad.State (StateT(..), evalStateT, lift, get, MonadIO, liftIO, runState, modify, evalState)
import Control.Monad.Reader (runReaderT)
import Blob.REPL.Types (REPLError, REPLState(..), REPL, Command(..), InlineCode(..))
import Blob.Inference.Types (GlobalEnv(..), TypeEnv(..))
import Blob.Parsing.Types(Program(..), Statement(..))
import Blob.Parsing.Defaults (initParseState)
import Blob.REPL.DefaultEnv (defaultEnv, defaultDeclContext, defaultDefContext)
import Blob.REPL.Commands (command, helpCommand, exitCommand, evaluate)
import Blob.Parsing.Parser (program)
import Blob.Inference.AlgorithmW (programTypeInference, tiProgram, checkTI, typeInference)
import Blob.PrettyPrinter.PrettyParser (pExpression)
import Blob.PrettyPrinter.PrettyInference (pType)
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Void (Void)
import qualified Data.Text as Text (Text, pack)
import qualified Data.Text.IO as Text (readFile)
import System.Console.ANSI (setSGR, SGR(..), ColorIntensity(..), Color(..), ConsoleLayer(..))
import Text.Megaparsec.Error (ParseErrorBundle(..), errorBundlePretty, bundleErrors, parseErrorTextPretty)
import Text.Megaparsec (runParserT, PosState(..))
import Text.Megaparsec.Pos (SourcePos(..), unPos)
import System.IO (hFlush, stdout)
import Control.Monad (forever)
import System.Directory (doesFileExist)
import Data.List.NonEmpty (toList)
import Data.List.Utils (split)
import Data.String.Utils (rstrip)
import Data.Char (toUpper)

runREPL :: REPL a -> IO (Either REPLError a)
runREPL r = do
    liftIO $ do
        replSetColor Vivid White >> putStr "Blob v0.0.1\nType " >> setSGR [Reset]
        replSetColor Vivid Magenta >> putStr "“:?”" >> setSGR [Reset]
        replSetColor Vivid White >> putStrLn " for a list of commands." >> setSGR [Reset]
        hFlush stdout

    runExceptT (evalStateT (runInputT defaultSettings r) initREPLState)
  where initREPLState = REPLState { ctx = initGlobalEnv
                                  , op = initParseState
                                  , values = defaultEnv }
    
initGlobalEnv :: GlobalEnv
initGlobalEnv = GlobalEnv { declCtx = TypeEnv defaultDeclContext
                          , defCtx = TypeEnv defaultDefContext }

replLoop :: REPL ()
replLoop = forever $ do
    replSetColor Vivid Blue
    input <- getInputLine "β> "
    liftIO $ setSGR [Reset]
    
    case input of
        Nothing     -> liftIO $ putStr ""
        Just input' -> do
            env <- lift get
            let (output, state') = runState (runParserT command "" (Text.pack input')) (op env)

            lift . modify $ \st -> st { op = state' }

            replCheck output

replCheck :: Either (ParseErrorBundle Text.Text Void) Command -> REPL ()
replCheck = \case
    Left err -> liftIO $ replSetColor Vivid Red >> replErrorPretty err >> setSGR [Reset] >> hFlush stdout
    Right c  -> case c of
        Help                -> liftIO helpCommand
        Exit                -> liftIO exitCommand
        Reload              ->
            lift . modify $ \st -> st { ctx = initGlobalEnv
                                      , op = initParseState
                                      , values = defaultEnv }
        Load file           -> do
            fileExists <- liftIO     $ doesFileExist file
            if fileExists
            then do
                content <- liftIO $ Text.readFile file
                st''    <- lift get
                let res = evalState (runParserT program file content) (op st'')
                case res of
                    Left err                  -> liftIO $ replSetColor Vivid Red >> putStr (errorBundlePretty err) >> setSGR [Reset] >> hFlush stdout
                    Right ast@(Program stmts) -> do
                        st <- lift get
                        case programTypeInference (ctx st) (tiProgram ast) of
                            Left err          -> liftIO $ replSetColor Vivid Red >> putStr (show err) >> setSGR [Reset] >> hFlush stdout
                            Right (_, state') -> do
                                lift . modify $ \st' -> st' { ctx = state' }

                                mapM_ (\case
                                        Definition id' expr -> do
                                            let eval' = runExcept $ runReaderT (evaluate expr) (values st)

                                            case eval' of
                                                Left err      -> liftIO $ replSetColor Vivid Red >> putStr (show err) >> setSGR [Reset] >> hFlush stdout
                                                Right evalRes -> lift . modify $ \st' -> st' { values = Map.insert id' evalRes (values st') }
                                        _                   -> liftIO $ putStr ""
                                    ) stmts
            else
                liftIO $ replSetColor Vivid Red >> putStrLn ("Unknown file `" ++ file ++ "`. Does it exist?") >> setSGR [Reset] >> hFlush stdout
        GetType e           -> do
            st <- lift get
            let (TypeEnv e1_) = declCtx $ ctx st
                (TypeEnv e2_) = defCtx $ ctx st
                env = Map.unionWith const e1_ e2_
                t = runExcept (evalStateT (checkTI $ typeInference (TypeEnv env) e) (ctx st))
            case t of
                Left err    -> liftIO $ replSetColor Vivid Red >> putStr (show err) >> setSGR [Reset] >> hFlush stdout
                Right type' ->  liftIO $ replSetColor Vivid Yellow >> putStr (show (pExpression e (0 :: Int))) >> setSGR [Reset] >> putStr " :: " >> replSetColor Vivid Cyan >> print (pType type') >> setSGR [Reset] >> hFlush stdout
        Code (CStatement s) -> do
            st <- lift get
            case programTypeInference (ctx st) (tiProgram $ Program [s]) of
                Left err -> liftIO $ replSetColor Vivid Red >> putStr (show err) >> setSGR [Reset] >> hFlush stdout
                Right (_, state') -> do
                    lift . modify $ \st' -> st' { ctx = state' }

                    case s of
                        Definition id' expr -> do
                            st' <- lift get
                            let eval' = runExcept $ runReaderT (evaluate expr) (values st')

                            case eval' of
                                Left err -> liftIO $ replSetColor Vivid Red >> putStr (show err) >> setSGR [Reset] >> hFlush stdout
                                Right evalRes -> lift . modify $ \st' -> st' { values = Map.insert id' evalRes (values st') }
                        _                   -> pure ()

                    liftIO $ replSetColor Dull Green >> putStr "" >> setSGR [Reset] >> hFlush stdout
        Eval e              -> do
            st <- lift get
            let (TypeEnv e1_) = declCtx $ ctx st
                (TypeEnv e2_) = defCtx $ ctx st
                env = Map.unionWith const e1_ e2_
                t = runExcept (evalStateT (checkTI $ typeInference (TypeEnv env) e) (ctx st))
            case t of
                Left err -> liftIO $ replSetColor Vivid Red >> putStr (show err) >> setSGR [Reset] >> hFlush stdout
                Right _  -> do
                    let res = runExcept $ runReaderT (evaluate e) (values st)

                    case res of
                        Left err      -> liftIO $ replSetColor Vivid Red >> print err >> setSGR [Reset] >> hFlush stdout
                        Right evalRes -> liftIO $ replSetColor Vivid Cyan >> print evalRes >> setSGR [Reset] >> hFlush stdout





replSetColor :: MonadIO m => ColorIntensity -> Color -> m ()
replSetColor intensity color = liftIO $ setSGR [SetColor Foreground intensity color]

-- replErrorPretty :: ParseErrorBundle Text.Text Void -> IO ()
-- replErrorPretty bundle = do
--     let errors = toList $ bundleErrors bundle
--         texts  = flip List.map errors $ \e -> do (PosState _ _ pos _ _) <- bundlePosState bundle
--                                                  (SourcePos name line col) <- pos
--                                                  "at <“" ++ name ++ "”:" ++ show line ++ ":" ++ show col ++ ">\n" ++ parseErrorTextPretty e
--         lines' = List.map (split "\n") texts
--         lines''= List.map (filter (/= "")) lines'
--         errs   = List.map (List.map (capitalize . rstrip . flip (++) ".")) lines''
--         errs'  = List.map (List.intercalate "\n") errs
--     mapM_ putStr errs' >> putStr "\n"

replErrorPretty :: ParseErrorBundle Text.Text Void -> IO ()
replErrorPretty bundle = do
    let (PosState _ _ state _ _) = bundlePosState bundle
        errors = map (, state) (toList $ bundleErrors bundle)
        texts  = flip List.map errors $ \(e, pos) -> do let (SourcePos name line col) = pos
                                                        "at <“" ++ name ++ "”:" ++ show (unPos line) ++ ":" ++ show (unPos col) ++ ">\n" ++ parseErrorTextPretty e
        lines_ = List.map (split "\n") texts
        lines' = List.map (filter (/= "")) lines_
        errs   = List.map (List.map (capitalize . rstrip . flip (++) ".")) lines'
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