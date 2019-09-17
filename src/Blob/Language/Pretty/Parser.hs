{-# LANGUAGE BlockArguments, LambdaCase #-}

module Blob.Language.Pretty.Parser
( pProgram
, pStatement
, pExpression
, pType
) where

import Blob.Language.Desugaring.Types (Program(..), Statement(..), Type(..), Expr(..), Literal(..), Pattern(..), Scheme(..), CustomType(..))
import Blob.Language.Parsing.Types (Associativity(..), Fixity(..))
import Text.PrettyPrint.Leijen hiding ((<$>))
import qualified Text.PrettyPrint.Leijen as PP ((<$>))
import Data.List (intersperse)
import qualified Data.Map as Map (toList, map, foldr)
import Blob.Language.Parsing.Annotation

indentLevel :: Int
indentLevel = 4

pProgram :: Program -> Doc
pProgram (Program []) = text "Program []"
pProgram (Program stt) =
    let printStatement s = indent indentLevel (pStatement s)
        printStatements s = mconcat $ map (flip (<>) linebreak . printStatement) s
    in text "Program [" <$$> printStatements stt <> text "]"

pStatement :: Annotated Statement -> Doc
pStatement (Declaration name t :- _) =
    text "Declaration:" <$$> indent indentLevel (text "Id = \"" <> text name <> text "\"" <$$> text "Type = " <> pType t)
pStatement (Definition name e :- _) =
    text "Definition:" <$$> indent indentLevel (text "Id = \"" <> text name <> text "\"" <$$> text "Value = " <> pExpression e)
-- pStatement (OpFixity name fix) =
--     text "OpDeclaration:" <$$> indent indentLevel (text "Id = \"" <> text name <> text "\"" <$$> text "Fixity = " <> pFixity fix)
pStatement (TypeDeclaration name _ custom :- _) =
    let printCustomType = \case
                              TAlias t -> pType t
                              TSum map' -> indent indentLevel $ Map.foldr (\t acc -> acc <> linebreak <> pCtor t) (text "") map'
                        where pCtor (Scheme _ t) = pType t
        printCustom = \case
                          t@(TAlias _) :- _ -> text "Alias = " <> printCustomType t
                          t@(TSum _) :- _ -> text "Constructors = " <$$> printCustomType t
    in text "TypeDeclaration:" <$$> indent indentLevel (text "Id = \"" <> text name <> text "\"" <$$> printCustom custom)
pStatement _ = undefined

pFixity :: Fixity -> Doc
pFixity (Infix assoc prec _) =
    let printAssoc = \case
                         L -> text "infixl"
                         R -> text "infixr"
                         _ -> text "infix"
    in printAssoc assoc <+> text (show prec)

pExpression :: Annotated Expr -> Doc
pExpression (ELit l :- _) = pLiteral l
  where pLiteral (LDec d) = text (show d)
        pLiteral (LInt i) = text (show i)
        pLiteral (LChr c) = text (show c)
pExpression (EId i :- _) = text i
pExpression (EHole :- _) = text "_"
pExpression (ELam arg e :- _) = parens $ text "\\" <+> text arg <+> text "->" <+> pExpression e
pExpression (ETuple e :- _) = parens . mconcat $ intersperse (text ", ") (map pExpression e)
pExpression (EMatch toMatch cases :- _) =
    let printCase (pat, expr) = pPattern pat <+> text "->" <+> pExpression expr
        printCases c = mconcat $ map (flip (<>) linebreak . printCase) c
    in text "Match" <+> pExpression toMatch <+> text "=" <$$> indent indentLevel (printCases cases)
pExpression (EApp e1 e2 :- _) = pExpression e1 <+> parenthesizeIfNeeded e2
  where parenthesizeIfNeeded (p :- p_) = case p of
            EApp _ _ -> parens $ pExpression (p :- p_)
            EMatch{} -> parens $ pExpression (p :- p_)
            _ -> pExpression (p :- p_)
pExpression (EAnn e t :- _) = parens $ pExpression e <+> text "::" <+> pType t

pPattern :: Annotated Pattern -> Doc
pPattern (PInt i :- _) = text (show i)
pPattern (PDec d :- _) = text (show d)
pPattern (PChr c :- _) = text (show c)
pPattern (PId i :- _) = text i
pPattern (Wildcard :- _) = text "_"
pPattern (PTuple pats :- _) = parens . mconcat $ intersperse (text ", ") (map pPattern pats)
pPattern (PAnn p t :- _) = parens $ pPattern p <+> text "::" <+> pType t
pPattern (PCtor name args :- _) =
    let parenthesized = foldr ((<+>) . parenthesizeIfNeeded) empty args
    in text name <+> parenthesized
  where parenthesizeIfNeeded (p :- p_) = case p of
            PCtor _ _ -> parens (pPattern (p :- p_))
            _ -> pPattern (p :- p_)

pType :: Annotated Type -> Doc
pType (t :- _) = case t of
    TId i -> text i
    TVar tv -> text tv
    TTuple ts -> parens . mconcat $ intersperse (text ", ") (map pType ts)
    TApp t1 t2 -> pType t1 <+> parenthesizeIfNeeded t2
    TFun t1 t2 -> parenthesizeIfNeededF t1 <+> text "->{?}" <+> parenthesizeIfNeededF t2
    TArrow n t1 t2 -> parenthesizeIfNeededF t1 <+> text "->{" <> pExpression n <> text "}" <+> parenthesizeIfNeededF t2
  where parenthesizeIfNeeded (t :- p) = case t of
            TApp _ _ -> parens $ pType (t :- p)
            TFun _ _ -> parens $ pType (t :- p)
            TArrow{} -> parens $ pType (t :- p)
            _ -> pType (t :- p)
        parenthesizeIfNeededF (t :- p) = case t of
            TFun _ _ -> parens $ pType (t :- p)
            TArrow{} -> parens $ pType (t :- p)
            _ -> pType (t :- p)