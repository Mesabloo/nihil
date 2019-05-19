{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}

module Blob.Parsing.Defaults where

import Blob.Parsing.Types (ParseState(..), Parser, Expr(..), CustomOperator(..), Fixity(..), Associativity(..), Operator(..))
import qualified Data.MultiMap as MMap
import Control.Monad.State (lift, modify, get)
import Data.Text (unpack)
import Blob.Parsing.Lexer (symbol, indentGuard, space', sameOrIndented, same, indented, scN)
import Text.Megaparsec.Pos (mkPos)
import Text.Megaparsec (optional, (<|>), (<?>), try)
import Text.Megaparsec.Char (eol)
import Text.Megaparsec.Char.Lexer (indentLevel)
import qualified Data.Text as Text (Text)
import Data.Functor (($>))

defaultOps :: MMap.MultiMap Integer (Operator Parser Expr)
defaultOps = MMap.fromList [
                  (9, InfixL . try $ pure EApp)
                , (7, toParser $ CustomOperator "*" (Infix' L 7))
                , (7, toParser $ CustomOperator "/" (Infix' L 7))
                , (6, toParser $ CustomOperator "+" (Infix' L 6))
                , (6, toParser $ CustomOperator "-" (Infix' L 6))
            ]

initParseState :: ParseState
initParseState = ParseState defaultOps (mkPos 1)

addOperator :: CustomOperator -> Parser ()
addOperator fixdecl = m fixdecl (fixityPrec fixdecl)
  where m f p = modify $ \st -> st { operators = MMap.insert p (toParser f) (operators st) }

fixityPrec :: CustomOperator -> Integer
fixityPrec (CustomOperator _ (Infix' _ n)) = n
fixityPrec (CustomOperator _ (Prefix'  n)) = n
fixityPrec (CustomOperator _ (Postfix' n)) = n

toParser :: CustomOperator -> Operator Parser Expr
toParser (CustomOperator name' fix') = case fix' of
    Infix' L _ -> InfixL . try $ do
        pos <- indentLevel
        sameOrIndented pos (try $ symbol name')
        pos' <- indentLevel
        sameOrIndented pos' . pure $ EApp . EApp (EId $ unpack name')
    Infix' R _ -> InfixR . try $ do
        pos <- indentLevel
        sameOrIndented pos (try $ symbol name')
        pos' <- indentLevel
        sameOrIndented pos' . pure $ EApp . EApp (EId $ unpack name')
    Infix' N _ -> InfixN . try $ do
        pos <- indentLevel
        sameOrIndented pos (try $ symbol name') <?> unpack name'
        pos' <- indentLevel
        sameOrIndented pos' . pure $ EApp . EApp (EId $ unpack name')
    Prefix'  _ -> Prefix . try $ do
        scN *> try (symbol name') <* scN
        pos <- indentLevel
        sameOrIndented pos . pure $ EApp (EId $ unpack name')
    Postfix' _ -> Postfix . try $ do
        pos <- indentLevel
        sameOrIndented pos (try $ symbol name') <* scN
        pure $ EApp (EId $ unpack name')