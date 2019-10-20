-- Blobc, a compiler for compiling Blob source code
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

{-# LANGUAGE BlockArguments, LambdaCase #-}

-- | This module contains all the pretty-printing functions concerning the AST.
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
import qualified Data.Map as Map (foldr)
import Blob.Language.Parsing.Annotation

-- | The default indentation level for the pretty printing
indentLevel :: Int
indentLevel = 4

-- | Program pretty printing
pProgram :: Program -> Doc
pProgram (Program []) = text "Program []"
pProgram (Program stt) =
    let printStatement s = indent indentLevel (pStatement s)
        printStatements s = mconcat $ map (flip (<>) linebreak . printStatement) s
    in text "Program [" <$$> printStatements stt <> text "]"

-- | Statement pretty printing
pStatement :: Annotated Statement -> Doc
pStatement (Declaration name t :- _) =
    text name <+> text "::" <+> pType t
pStatement (Definition name e :- _) =
    text name <+> text "=" <+> pExpression e
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
pStatement _ = text "-- Unimplemented pretty printing"

-- | Unused pretty printing
pFixity :: Fixity -> Doc
pFixity (Infix assoc prec _) =
    let printAssoc = \case
                         L -> text "infixl"
                         R -> text "infixr"
                         _ -> text "infix"
    in printAssoc assoc <+> text (show prec)

-- | Expression pretty printing
pExpression :: Annotated Expr -> Doc
pExpression (ELit l :- _) = pLiteral l
  where pLiteral (LDec d) = text (show d)
        pLiteral (LInt i) = text (show i)
        pLiteral (LChr c) = text (show c)
pExpression (ERead :- _) = text "read"
pExpression (EKill :- _) = text "kill"
pExpression (EDupl :- _) = text "dupl"
pExpression (EMake :- _) = text "make"
pExpression (EId i :- _) = text i
pExpression (EHole :- _) = text "_"
pExpression (ELam arg e :- _) = parens $ text "\\" <+> pPattern arg <+> text "->" <+> pExpression e
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
pExpression (ELet (s:ss) e :- _) = align (text "let" <+> align (foldl (PP.<$>) (pStatement s) (pStatement <$> ss)) PP.<$> (text "in" <+> pExpression e))
pExpression _ = text "error: the impossible happened while pretty printing!"

-- | Pattern pretty printing
pPattern :: Annotated Pattern -> Doc
pPattern (PInt i :- _) = text (show i)
pPattern (PDec d :- _) = text (show d)
pPattern (PChr c :- _) = text (show c)
pPattern (PId i :- _) = text i
pPattern (Wildcard :- _) = text "_"
pPattern (PTuple pats :- _) = parens . mconcat $ intersperse (text ", ") (map pPattern pats)
pPattern (PAnn p t :- _) = parens $ pPattern p <+> text "::" <+> pType t
pPattern (PLinear p :- _) = text "!" <> pPattern p
pPattern (PCtor name args :- _) =
    let parenthesized = foldr ((<+>) . parenthesizeIfNeeded) empty args
    in text name <+> parenthesized
  where parenthesizeIfNeeded (p :- p_) = case p of
            PCtor _ _ -> parens (pPattern (p :- p_))
            _ -> pPattern (p :- p_)

-- | Type pretty printing
pType :: Annotated Type -> Doc
pType (t :- _) = case t of
    TId i -> text i
    TVar tv -> text tv
    TTuple ts -> parens . mconcat $ intersperse (text ", ") (map pType ts)
    TApp t1 t2 -> pType t1 <+> parenthesizeIfNeeded t2
    TFun t1 t2 -> parenthesizeIfNeededF t1 <+> text "-o" <+> parenthesizeIfNeededF t2
    TBang t1 -> text "!" <> parenthesizeIfNeeded t1
  where parenthesizeIfNeeded (t :- p) = case t of
            TApp _ _ -> parens $ pType (t :- p)
            TFun _ _ -> parens $ pType (t :- p)
            _ -> pType (t :- p)
        parenthesizeIfNeededF (t :- p) = case t of
            TFun _ _ -> parens $ pType (t :- p)
            _ -> pType (t :- p)