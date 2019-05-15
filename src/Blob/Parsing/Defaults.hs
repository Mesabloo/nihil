{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}

module Blob.Parsing.Defaults where

import Blob.Parsing.Types (ParseState(..), Parser, Expr(..), CustomOperator(..), Fixity(..), Associativity(..))
import Control.Monad.Combinators.Expr (Operator(..))
import qualified Data.MultiMap as MMap
import Control.Monad.State (lift, modify, get)
import Data.Text (unpack)
import Blob.Parsing.Lexer (string, indentGuard)
import Text.Megaparsec.Pos (mkPos)
import Text.Megaparsec (optional, (<|>))
import Text.Megaparsec.Char (eol)
import qualified Data.Text as Text (Text)
import Data.Functor (($>))

defaultOps :: MMap.MultiMap Integer (Operator Parser Expr)
defaultOps = MMap.fromList [
                  (7, toParser $ CustomOperator "*" (Infix' L 7))
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
    Infix' L _ -> InfixL $ 
        string name' $> \exp1 -> EApp (EApp (EId $ unpack name') exp1)
    Infix' R _ -> InfixR $
        string name' $> \exp1 -> EApp (EApp (EId $ unpack name') exp1)
    Infix' N _ -> InfixN $
        string name' $> \exp1 -> EApp (EApp (EId $ unpack name') exp1)
    Prefix'  _ -> Prefix $
        EApp (EId $ unpack name') <$ string name'
    Postfix' _ -> Postfix $
        EApp (EId $ unpack name') <$ string name'