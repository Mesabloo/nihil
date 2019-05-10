{-# LANGUAGE BlockArguments #-}

module Blob.PrettyPrinter.PrettyParser
( pProgram
, pStatement
, pExpression
, pType
) where

import Blob.Parsing.Types (Program(..), Statement(..), Type(..), Expr(..), Literal(..), Associativity(..), Fixity(..), Pattern(..), CustomType(..))
import Text.PrettyPrint.Leijen (text, parens, indent, line, (<$$>), (<>), empty, brackets, Doc, (<+>), punctuate)
import qualified Text.PrettyPrint.Leijen as PP ((<$>))
import Data.List (intersperse)
import qualified Blob.PrettyPrinter.PrettyInference as PI (pType)
import qualified Data.Map as Map (toList)
import qualified Blob.Inference.Types as I (Scheme(..))

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
pStatement (TypeDeclaration ct) i      =
    indent i $ case ct of
        (TSum name cs) -> text "SumType" <$$> printDetails name (Map.toList cs)
        (TProd name c) -> text "ProductType" <$$> printDetails name [c]
  where printDetails name cs         = indent indentLevel (text "Constructors ["
                                            <$$> indent indentLevel (pCtor (head cs) indentLevel <> mconcat (map (\c -> line <> pCtor c indentLevel) (tail cs)))
                                            <$$> text "]")
        pCtor (name, I.Scheme _ t) i = text name <> text " :: " <> PI.pType t 
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
    EApp exp1 exp2   -> parens $ pExpression exp1 i <+> pExpression exp2 i
    ELam arg exp0    -> parens $ text "λ " <> text arg <> text " → " <> pExpression exp0 i
    ETuple es        -> parens . mconcat $ intersperse (text ", ") (map (`pExpression` i) es)
    EList es         -> brackets . mconcat $ intersperse (text ", ") (map (`pExpression` i) es)
    EMatch e pats    -> parens $ text "match " <> pExpression e i <> mconcat (punctuate line (map (\(a, b) -> indent indentLevel $ pPattern a i <> text " → " <> pExpression b i) pats))

pPattern :: Pattern -> Int -> Doc
pPattern p i = case p of
    Wildcard -> text "_"
    PInt i   -> text $ show i
    PDec d   -> text $ show d
    PStr s   -> text s

pType :: Type -> Int -> Doc
pType t i = case t of
    TId t'              -> text t'
    TVar t'             -> text t'
    TTuple ts           -> parens . mconcat $ intersperse (text ", ") (map (`pType` i) ts)
    TArrow count t' t'' -> parens $ brackets (pExpression count i) <> pType t' i <> text " → " <> pType t'' i
    TList t             -> brackets $ pType t i
    TApp t1 t2          -> pType t1 i <+> pType t2 i