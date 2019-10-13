-- iBlob, a REPL using the Blob programming language's interpreter.
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

-- | This module contains all the functions executed for each command.
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
import Text.Megaparsec (eof, try, eitherP)
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
import Control.Lens hiding (op)

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
        content       <- liftIO $ Text.readFile file
        st''          <- lift get
        tks           <- rethrowEither (text . errorBundlePretty) (runLexer content file)
        parseTree     <- rethrowEither printParseError (runParser tks file)

        let state' = st'' ^. op
        (ast, state') <- rethrowEither id $ runSugar (runDesugarer file (parseTree :- Nothing)) state'

        op .= state'
        (_, state')   <- rethrowEither id $ runTypeInference (st'' ^. ctx) (tiProgram ast)
        ctx .= state'

        let (Program stmts) = getAnnotated ast

        forM_ stmts $ \case
            Definition id' expr :- _ -> do
                st'' <- get
                evalRes <- lift . lift $ runReaderT (evaluate expr) (st'' ^. values)

                values . vals %= Map.insert id' evalRes
            _                   -> pure ()

getType :: String -> REPL ()
getType expr = do
    st <- get
    tks <- rethrowEither (text . errorBundlePretty) $ runLexer (Text.pack expr) "interactive"

    e <- rethrowEither printParseError $ runParser' (expression <* eof) tks "interactive"

    (e, _) <- rethrowEither id $ runSugar
        (do { accumulateOnExpression e
            ; desugarExpression "interactive" e } ) (st ^. op)

    (Scheme _ type') <- rethrowEither id $ inferExpr (st ^. ctx) e

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
    st <- get

    tks <- rethrowEither (text . errorBundlePretty) $ runLexer (Text.pack typeExpr) "interactive"

    t <- rethrowEither printParseError $ runParser' (type' <* eof) tks "interactive"

    (t, _) <- rethrowEither id $ runSugar (desugarType "interactive" t) (st ^. op)

    let t1 = tiType t
    kind <- rethrowEither id $ runExcept (evalStateT (checkKI $ kindInference (st ^. ctx . typeDeclCtx) t1) (st ^. ctx))

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
    st <- get
    x <- rethrowEither (text . errorBundlePretty) $ runLexer (Text.pack stat) "interactive"

    rethrowEither printParseError (runParser' (eitherP (try (expression <* eof)) (program <* eof)) x "interactive")
     >>= \case
        Left e -> do
            (e, _) <- rethrowEither id $ runSugar
                ( do { accumulateOnExpression e
                     ; desugarExpression "interactive" e } ) (st ^. op)

            rethrowEither id $ inferExpr (st ^. ctx) e

            evalRes <- lift . lift $ runReaderT (evaluate e) (st ^. values)

            liftIO $ setSGR [SetColor Foreground Vivid Cyan]
                >> print evalRes
                >> setSGR [Reset]
                >> hFlush stdout

        Right p  -> do
            (p'@(Program ss :- _), state') <- rethrowEither id $ runSugar (runDesugarer "interactive" (p :- Nothing)) (st ^. op)

            op .= state'
            (_, state') <- rethrowEither id $ runTypeInference (st ^. ctx) (tiProgram p')
            ctx .= state'

            forM_ ss $ \case
                Definition id' expr :- _ -> do
                    st'   <- get
                    evalRes <- lift . lift $ runReaderT (evaluate expr) (st' ^. values)

                    values . vals %= Map.insert id' evalRes
                _                   -> pure ()

execTime :: String -> REPL ()
execTime expr = do
    st <- get
    tks <- rethrowEither (text . errorBundlePretty) $ runLexer (Text.pack expr) "interactive"

    e <- rethrowEither printParseError $ runParser' (try expression <* eof) tks "interactive"

    (e, _) <- rethrowEither id $ runSugar
        ( do { accumulateOnExpression e
             ; desugarExpression "interactive" e } ) (st ^. op)

    rethrowEither id $ inferExpr (st ^. ctx) e

    (t, res) <- liftIO . time . runExceptT $ runReaderT (evaluate e) (st ^. values)
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
    st <- get

    tks <- rethrowEither (text . errorBundlePretty) $ runLexer (Text.pack expr) "interactive"

    e <- rethrowEither printParseError $ runParser' (try expression <* eof) tks "interactive"

    (e, _) <- rethrowEither id $ runSugar
        ( do { accumulateOnExpression e
             ; desugarExpression "interactive" e } ) (st ^. op)

    rethrowEither id $ inferExpr (st ^. ctx) e

    t' <- liftIO . replicateM (fromIntegral n) . time . runExceptT $ runReaderT (evaluate e) (st ^. values)

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
    st  <- use ctx
    let types           = Map.toList $ st ^. typeDeclCtx
        (TypeEnv funs)  = st ^. defCtx <> st ^. ctorCtx
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
    ctx .= initGlobalEnv
    values .= initEvalState

    reload
resetEnv [x] _ = resetOne x
resetEnv (x:xs) r = resetOne x *> resetEnv xs r

resetOne :: String -> REPL ()
resetOne x = do
    ctx . defCtx %= (TypeEnv . Map.delete x . getMap)
    values . vals %= Map.delete x




-- | A simple function to get the execution time of an action.
time :: IO a -> IO (Double, a)
time f = do
    begin  <- getTime
    result <- f
    end    <- getTime
    pure $ (,) (end - begin) result

printParseError :: (Stream s, ShowErrorComponent e) => ParseErrorBundle s e -> Doc
printParseError = text . concatMap parseErrorTextPretty . bundleErrors

-- | Rethrows the 'Either' computation, applying the function on the error, or returning the result.
rethrowEither :: (e -> REPLError) -> Either e a -> REPL a
rethrowEither f = either (lift . throwError . f) pure
