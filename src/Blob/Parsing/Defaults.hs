{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}

module Blob.Parsing.Defaults where

import Blob.Parsing.Types (ParseState(..), Parser, Expr(..), CustomOperator(..), Fixity(..), Associativity(..))
import Control.Monad.Combinators.Expr (Operator(..))
import qualified Data.MultiMap as MMap
import Control.Monad.State (lift, modify)
import Data.Text (unpack)
import Blob.Parsing.Lexer (string)
import Text.Megaparsec.Pos (mkPos)

defaultOps :: MMap.MultiMap Integer (Operator Parser Expr)
defaultOps = MMap.fromList [
                  (7, toParser $ CustomOperator "*" (Infix' L 7))
                , (7, toParser $ CustomOperator "/" (Infix' L 7))
                , (6, toParser $ CustomOperator "+" (Infix' L 6))
                , (6, toParser $ CustomOperator "-" (Infix' L 6))
            ]

initParseState :: ParseState
initParseState = ParseState defaultOps (mkPos 0)

addOperator :: CustomOperator -> Parser ()
addOperator fixdecl = m fixdecl (fixityPrec fixdecl)
  where m f p = lift . modify $ \st -> st { operators = MMap.insert p (toParser f) (operators st) }

fixityPrec :: CustomOperator -> Integer
fixityPrec (CustomOperator _ (Infix' _ n)) = n
fixityPrec (CustomOperator _ (Prefix'  n)) = n
fixityPrec (CustomOperator _ (Postfix' n)) = n

toParser :: CustomOperator -> Operator Parser Expr
toParser (CustomOperator name' fix') = case fix' of
    Infix' L _ -> InfixL $
        (\exp1 exp2 -> EApp (EApp (EId $ unpack name') exp1) exp2) <$ (string name')
    Infix' R _ -> InfixR $
        (\exp1 exp2 -> EApp (EApp (EId $ unpack name') exp1) exp2) <$ (string name')
    Infix' N _ -> InfixN $
        (\exp1 exp2 -> EApp (EApp (EId $ unpack name') exp1) exp2) <$ (string name')
    Prefix'  _ -> Prefix $
        (EApp (EId $ unpack name')) <$ (string name')
    Postfix' _ -> Postfix $
        (EApp (EId $ unpack name')) <$ (string name')