{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}

module Blob.Parsing.Defaults where

import Blob.Parsing.Types (ParseState(..), Parser, Expr(..), CustomOperator(..), Fixity(..), Associativity(..), Operator(..), Literal(..))
import qualified Data.MultiMap as MMap
import Control.Monad.State (lift, modify, get)
import Data.Text (unpack)
import Blob.Parsing.Lexer (symbol, indentGuard, space', sameOrIndented, same, indented, scN, opSymbol, keySymbol)
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
                , (5, toParser $ CustomOperator ":" (Infix' R 5))
                , (4, negate')
                -- , (-1, failingInfixOperator)
                -- , (-1, failingPrefixOperator)
                -- , (-1, failingPostfixOperator)
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

negate' :: Operator Parser Expr
negate' = Prefix $ keySymbol "-" $> EApp (EApp (EId "-") (ELit $ LInt 0))

toParser :: CustomOperator -> Operator Parser Expr
toParser (CustomOperator name' fix') = case fix' of
    Infix' L _ -> InfixL $ do
        pos <- indentLevel
        sameOrIndented pos (keySymbol name')
        pos' <- indentLevel
        sameOrIndented pos' . pure $ EApp . EApp (EId $ unpack name')
    Infix' R _ -> InfixR $ do
        pos <- indentLevel
        sameOrIndented pos (keySymbol name')
        pos' <- indentLevel
        sameOrIndented pos' . pure $ EApp . EApp (EId $ unpack name')
    Infix' N _ -> InfixN $ do
        pos <- indentLevel
        sameOrIndented pos (keySymbol name') <?> unpack name'
        pos' <- indentLevel
        sameOrIndented pos' . pure $ EApp . EApp (EId $ unpack name')
    Prefix'  _ -> Prefix $ do
        keySymbol name'
        pure . EApp . EId $ unpack name'
    Postfix' _ -> Postfix $ do
        keySymbol name'
        pure . EApp . EId $ unpack name'

-- failingInfixOperator :: Operator Parser Expr
-- failingInfixOperator = InfixN $ do
--     o   <- opSymbol <* scN
--     fail $ "Unknown infix operator “" <> o <> "”."

-- failingPrefixOperator :: Operator Parser Expr
-- failingPrefixOperator = Prefix $ do
--     o   <- opSymbol <* scN
--     fail $ "Unknown prefix operator “" <> o <> "”."

-- failingPostfixOperator :: Operator Parser Expr
-- failingPostfixOperator = Postfix $ do
--     o   <- opSymbol <* scN
--     fail $ "Unknown postfix operator “" <> o <> "”."