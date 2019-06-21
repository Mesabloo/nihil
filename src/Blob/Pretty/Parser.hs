{-# LANGUAGE BlockArguments, LambdaCase #-}

module Blob.Pretty.Parser
( pProgram
, pStatement
, pExpression
, pType
) where

import Blob.Parsing.Types (Program(..), Statement(..), Type(..), Expr(..), Literal(..), Associativity(..), Fixity(..), Pattern(..), Scheme(..), CustomType(..))
import Text.PrettyPrint.Leijen hiding ((<$>))
import qualified Text.PrettyPrint.Leijen as PP ((<$>))
import Data.List (intersperse)
import qualified Data.Map as Map (toList, map, foldr)
import qualified Blob.TypeChecking.Types as I (Scheme(..))

indentLevel :: Int
indentLevel = 4

pProgram :: Program -> Doc
pProgram (Program []) = text "Program []"
pProgram (Program stt) =
    let printStatement s = indent indentLevel (pStatement s)
        printStatements s = mconcat $ map (flip (<>) linebreak . printStatement) s
    in text "Program [" <$$> printStatements stt <> text "]"

pStatement :: Statement -> Doc
pStatement (Declaration name t) =
    text "Declaration:" <$$> indent indentLevel (text "Id = “" <> text name <> text "”" <$$> text "Type = " <> pType t)
pStatement (Definition name e) =
    text "Definition:" <$$> indent indentLevel (text "Id = “" <> text name <> text "”" <$$> text "Value = " <> pExpression e)
pStatement (OpDeclaration name fix) =
    text "OpDeclaration:" <$$> indent indentLevel (text "Id = “" <> text name <> text "”" <$$> text "Fixity = " <> pFixity fix)
pStatement (TypeDeclaration name _ custom) =
    let printCustomType = \case
                              TAlias t -> pType t
                              TSum map' -> indent indentLevel $ Map.foldr (\t acc -> acc <> linebreak <> pCtor t) (text "") map'
                        where pCtor (Scheme _ t) = pType t
        printCustom = \case
                          t@(TAlias _) -> text "Alias = " <> printCustomType t
                          t@(TSum _) -> text "Constructors = " <$$> printCustomType t
    in text "TypeDeclaration:" <$$> indent indentLevel (text "Id = “" <> text name <> text "”" <$$> printCustom custom)
pStatement _ = undefined

pFixity :: Fixity -> Doc
pFixity (Infix' assoc prec) =
    let printAssoc = \case
                         L -> text "infixl"
                         R -> text "infixr"
                         _ -> text "infix"
    in printAssoc assoc <+> text (show prec)
pFixity (Prefix' prec) =
    text (show prec)
pFixity (Postfix' prec) =
    text (show prec)

pExpression :: Expr -> Doc
pExpression (ELit l) = pLiteral l
  where pLiteral (LDec d) = text (show d)
        pLiteral (LInt i) = text (show i)
        pLiteral (LChr c) = text (show c)
pExpression (EId i) = text i
pExpression EHole = text "_"
pExpression (ELam arg e) = parens $ text "λ" <+> text arg <+> text "→" <+> pExpression e
pExpression (ETuple e) = parens . mconcat $ intersperse (text ", ") (map pExpression e)
pExpression (EMatch toMatch cases) =
    let printCase (pat, expr) = pPattern pat <+> text "→" <+> pExpression expr
        printCases c = mconcat $ map (flip (<>) linebreak . printCase) c
    in text "Match" <+> pExpression toMatch <+> text "=" <$$> indent indentLevel (printCases cases)
pExpression (EApp e1 e2) = parenthesizeIfNeeded e1 <+> parenthesizeIfNeeded e2
  where parenthesizeIfNeeded e@(EApp _ _) = parens $ pExpression e
        parenthesizeIfNeeded e@(EMatch _ _) = parens $ pExpression e
        parenthesizeIfNeeded p = pExpression p

pPattern :: Pattern -> Doc
pPattern (PInt i) = text (show i)
pPattern (PDec d) = text (show d)
pPattern (PChr c) = text (show c)
pPattern (PId i) = text i
pPattern Wildcard = text "_"
pPattern (PTuple pats) = parens . mconcat $ intersperse (text ", ") (map pPattern pats)
pPattern (PCtor name args) =
    let parenthesized = mconcat $ map parenthesizeIfNeeded args
    in text name <+> parenthesized
  where parenthesizeIfNeeded p = case p of
            PCtor _ _ -> parens (pPattern p)
            _ -> pPattern p

pType :: Type -> Doc
pType = \case
    TId i -> text i
    TVar tv -> text tv
    TTuple ts -> parens . mconcat $ intersperse (text ", ") (map pType ts)
    TApp t1 t2 -> parenthesizeIfNeeded t1 <+> parenthesizeIfNeeded t2
    TFun t1 t2 -> parenthesizeIfNeededF t1 <+> text "→{?}" <+> parenthesizeIfNeededF t2
    TArrow n t1 t2 -> parenthesizeIfNeededF t1 <+> text "→{" <> pExpression n <> text "}" <+> parenthesizeIfNeededF t2
  where parenthesizeIfNeeded t = case t of
            TApp _ _ -> parens $ pType t
            TFun _ _ -> parens $ pType t
            TArrow{} -> parens $ pType t
            _ -> pType t
        parenthesizeIfNeededF t = case t of
            TFun _ _ -> parens $ pType t
            TArrow{} -> parens $ pType t
            _ -> pType t