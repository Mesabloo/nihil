{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Main where

import Nihil.Syntax
import Control.Exception
import qualified Text.Megaparsec as MP
import qualified Data.Text.IO as T
import Nihil.Utils.Debug (log)
import Prelude hiding (log)

main :: IO ()
main = do
    let filename = "test"
        code     = "id : a -> a\nid x = x\nf = λ x -> id x + 1"

    putStrLn "Test parsing code:\n```"
    T.putStrLn code
    putStrLn "```\n"

    tks  <- log "Lexing code" $ rethrow (runLexer code filename)

    print tks

    ast  <- log "Parsing tokens" $ rethrow (runParser tks filename)

    print ast

    dAst <- log "Desugaring AST" $ rethrow' (runDesugarer ast)

    print dAst

    putStrLn "\nParsed AST:\n"
    putDoc (pretty ast)

    putStrLn "\n\nDesugared AST:\n"
    putDoc (pretty dAst)

rethrow :: (MP.Stream s, MP.ShowErrorComponent e) => Either (MP.ParseErrorBundle s e) a -> IO a
rethrow (Left err) = throw (ErrorCallWithLocation (MP.errorBundlePretty err) "")
rethrow (Right x)  = pure x

rethrow' :: Show e => Either e a -> IO a
rethrow' = either (throw . (`ErrorCallWithLocation` "") . show) pure