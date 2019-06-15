{-# LANGUAGE BlockArguments #-}

module Blob.PrettyPrinter.PrettyParser
( pProgram
, pStatement
, pExpression
, pType
) where

import Blob.Parsing.Types (Program(..), Statement(..), Type(..), Expr(..), Literal(..), Associativity(..), Fixity(..), Pattern(..), Scheme(..), CustomType(..))
import Text.PrettyPrint.Leijen (text, parens, indent, line, (<$$>), (<>), empty, brackets, Doc, (<+>), punctuate)
import qualified Text.PrettyPrint.Leijen as PP ((<$>))
import Data.List (intersperse)
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
    indent i $ text "Definition" <$$> indent indentLevel (text ("Id=" <> id') <$$> text "Value=" <> pExpression expr i)
pStatement (Declaration id' t) i       =
    indent i $ text "Declaration" <$$> indent indentLevel (text ("Id=" <> id') <$$> text "Type=" <> pType t i)
pStatement (OpDeclaration name' fix) i =
    indent i $ text "Operator" <$$> indent indentLevel (text ("Symbol=" <> name') <$$> text "Fixity=" <> pFixity fix i)
pStatement (TypeDeclaration name tvs ct) i      =
    indent i $ case ct of
        (TSum cs)   -> text "SumType" <$$> printDetails name (Map.toList cs)
        (TProd c s) -> text "ProductType" <$$> printDetails name [(c, s)]
  where printDetails name cs         = indent indentLevel (text "Constructors ["
                                            <$$> indent indentLevel (pCtor (head cs) indentLevel <> mconcat (map (\c -> line <> pCtor c indentLevel) (tail cs)))
                                            <$$> text "]")
        pCtor (name, Scheme _ t) i = text name <> text " :: " <> pType t i
pStatement Empty _ =
    empty

pFixity :: Fixity -> Int -> Doc
pFixity (Infix' a prec) _ = case a of
    L -> text $ "Infix - Left - " <> show prec
    R -> text $ "Infix - Right - " <> show prec
    N -> text $ "Infix - None - " <> show prec
pFixity (Prefix' prec) _  = text $ "Prefix - " <> show prec
pFixity (Postfix' prec) _ = text $ "Postfix - " <> show prec

pExpression :: Expr -> Int -> Doc
pExpression expr i = case expr of
    EId str          -> text str
    ELit (LStr str)  -> text $ show str
    ELit (LInt int') -> text $ show int'
    ELit (LDec dec') -> text $ show dec'
    ELit (LChr chr)  -> text $ show chr
    EApp exp1 exp2   -> parens $ pExpression exp1 i <+> pExpression exp2 i
    ELam arg exp0    -> parens $ text "λ " <> text arg <> text " → " <> pExpression exp0 i
    ETuple es        -> parens . mconcat $ intersperse (text ", ") (map (`pExpression` i) es)
    EMatch e pats    -> parens $ text "match " <> pExpression e i <> indent indentLevel (mconcat (punctuate line (map (\(a, b) -> pPattern a i <> text " → " <> pExpression b i) pats)))

pPattern :: Pattern -> Int -> Doc
pPattern p i = case p of
    Wildcard   -> text "_"
    PInt i     -> text $ show i
    PDec d     -> text $ show d
    PStr s     -> text $ show s
    PChr c     -> text $ show c
    PId i      -> text i
    PCtor i' a -> parens $ text i' <+> foldr ((<+>) . (`pPattern` i)) (text "") a

pType :: Type -> Int -> Doc
pType t i = case t of
    TId t'              -> text t'
    TVar t'             -> text t'
    TTuple ts           -> parens . mconcat $ intersperse (text ", ") (map (`pType` i) ts)
    TArrow count t' t'' -> parens $ brackets (pExpression count i) <> pType t' i <> text " → " <> pType t'' i
    TFun t1 t2          -> text "(" <> pType t1 i <> text " → " <> pType t2 i <> text ")"
    TApp t1 t2          -> pType t1 i <+> parens (pType t2 i)
