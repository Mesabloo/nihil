{-# LANGUAGE BlockArguments #-}

module Blob.PrettyPrinter.PrettyParser
( pProgram
, pStatement
, pExpression
, pType
) where

import Blob.Parsing.Types (Program(..), Statement(..), Type(..), Expr(..), Literal(..), Associativity(..), Fixity(..))
import Text.PrettyPrint.Leijen (text, parens, indent, line, (<$$>), (<>), empty, brackets, Doc)
import Data.List (intersperse)

indentLevel :: Int
indentLevel = 4

pProgram :: Program -> Doc
pProgram (Program [])    = text "Program []"
pProgram (Program stmts) = text "Program ["
    <$$> pStatement (head stmts) indentLevel <> mconcat (map (\s -> line <> pStatement s indentLevel) (tail stmts))
    <$$> text "]"

pStatement :: Statement -> Int -> Doc
pStatement (Definition id' expr) i     =
    indent i $ text "Definition" <$$> indent indentLevel (text ("Id=" ++ id') <$$> text "Value=" <> pExpression expr i)
pStatement (Declaration id' t) i       =
    indent i $ text "Declaration" <$$> indent indentLevel (text ("Id=" ++ id') <$$> text "Type=" <> pType t i)
pStatement (OpDeclaration name' fix) i =
    indent i $ text "Operator" <$$> indent indentLevel (text ("Symbol=" ++ name') <$$> text "Fixity=" <> pFixity fix i)
pStatement Empty _ =
    empty

pFixity :: Fixity -> Int -> Doc
pFixity (Infix' a prec) _ = case a of
        L -> text $ "Infix - Left - " ++ show prec
        R -> text $ "Infix - Right - " ++ show prec
        N -> text $ "Infix - None - " ++ show prec
pFixity (Prefix' prec) _  = text $ "Prefix - " ++ show prec
pFixity (Postfix' prec) _ = text $ "Postfix - " ++ show prec

pExpression :: Expr -> Int -> Doc
pExpression expr i = case expr of
        EId str          -> text str
        ELit (LStr str)  -> text $ show str
        ELit (LInt int') -> text $ show int'
        ELit (LDec dec') -> text $ show dec'
        EApp exp1 exp2   -> parens $ pExpression exp1 i <> text " " <> pExpression exp2 i
        ELam arg exp0    -> parens $ text "λ " <> text arg <> text " → " <> pExpression exp0 i
        ETuple es        -> parens . mconcat $ intersperse (text ", ") (map (`pExpression` i) es)
        EList es         -> brackets . mconcat $ intersperse (text ", ") (map (`pExpression` i) es)

pType :: Type -> Int -> Doc
pType t i =
    case t of
        TId t'              -> text t'
        TVar t'             -> text t'
        TTuple ts           -> parens . mconcat $ intersperse (text ", ") (map (`pType` i) ts)
        TArrow count t' t'' -> parens $ brackets (pExpression count i) <> pType t' i <> text " → " <> pType t'' i
        TList t             -> brackets $ pType t i