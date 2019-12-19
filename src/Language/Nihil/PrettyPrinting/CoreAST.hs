{-# LANGUAGE LambdaCase, FlexibleInstances #-}

module Language.Nihil.PrettyPrinting.CoreAST where

import Language.Nihil.Syntax.Internal.Desugaring.CoreAST
import Language.Nihil.Syntax.Internal.Parsing.Located
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))
import qualified Text.PrettyPrint.ANSI.Leijen as PP ((<$>))
import qualified Data.Map as Map

-- | The default indentation level for the pretty printing
indentLevel :: Int
indentLevel = 4

instance Pretty Program where
    -- | Program pretty printing
    pretty (Program []) = text "Program []"
    pretty (Program stt) =
        let printStatement s = indent indentLevel (pretty s)
            printStatements s = mconcat $ map ((linebreak <>) . printStatement) s
        in text "Program [" <$$> printStatements stt <> rbracket

instance Pretty (Located Statement) where
    -- | Statement pretty printing
    pretty (Declaration name t :@ _) =
        text name <+> text "::" <+> pretty t
    pretty (Definition name e :@ _) =
        text name <+> text "=" <+> pretty e
    pretty (TypeDeclaration name _ custom :@ _) =
        let printCustomType = \case
                TAlias t -> pretty t
                TSum map' -> indent indentLevel $ Map.foldr (\t acc -> acc <> linebreak <> pCtor t) empty map'
              where pCtor (Scheme _ t) = pretty t
            printCustom = \case
                t@(TAlias _) :@ _ -> text "Alias = " <> printCustomType t
                t@(TSum _) :@ _ -> text "Constructors = " <$$> printCustomType t
        in text "TypeDeclaration:" <$$> indent indentLevel (text "Id = \"" <> text name <> text "\"" <$$> printCustom custom)
    pretty _ = text "-- Unimplemented pretty printing"

instance Pretty (Located Expr) where
    -- | Expression pretty printing
    pretty (ELit l :@ _) = pretty l
    pretty (EId i :@ _) = text i
    pretty (EHole :@ _) = text "_"
    pretty (ELam arg e :@ _) =
        parens $ text "\\" <+> pretty arg <+> text "->" <+> pretty e
    pretty (ETuple e :@ _) = tupled (pretty <$> e)
    pretty (EMatch toMatch cases :@ _) =
        let printCase (pat, expr) = pretty pat <+> text "->" <+> pretty expr
            printCases c = mconcat $ map (flip (<>) linebreak . printCase) c
        in text "Match" <+> pretty toMatch <+> text "=" <$$> indent indentLevel (printCases cases)
    pretty (EApp e1 e2 :@ _) = pretty e1 <+> parenthesizeIfNeeded e2
        where parenthesizeIfNeeded (p :@ p_) = case p of
                    EApp _ _ -> parens $ pretty (p :@ p_)
                    EMatch{} -> parens $ pretty (p :@ p_)
                    _ -> pretty (p :@ p_)
    pretty (EAnn e t :@ _) = parens $ pretty e <+> text "::" <+> pretty t
    pretty (ELet (s:ss) e :@ _) =
        align (text "let" <+> align (foldl (PP.<$>) (pretty s) (pretty <$> ss)) PP.<$> (text "in" <+> pretty e))
    pretty _ = text "-- Unimplemented pretty printing"

instance Pretty Literal where
    -- | Literal pretty printing
    pretty (LDec d) = text (show d)
    pretty (LInt i) = text (show i)
    pretty (LChr c) = text (show c)

instance Pretty (Located Pattern) where
    -- | Pattern pretty printing
    pretty (PInt i :@ _) = integer i
    pretty (PDec d :@ _) = double d
    pretty (PChr c :@ _) = squotes (char c)
    pretty (PId i :@ _) = text i
    pretty (Wildcard :@ _) = text "_"
    pretty (PTuple pats :@ _) = tupled (pretty <$> pats)
    pretty (PAnn p t :@ _) = parens $ pretty p <+> text "::" <+> pretty t
    pretty (PCtor name args :@ _) =
        let parenthesized = foldr ((<+>) . parenthesizeIfNeeded) empty args
        in text name <+> parenthesized
      where parenthesizeIfNeeded (p :@ p_) = case p of
                PCtor _ _ -> parens (pretty (p :@ p_))
                _ -> pretty (p :@ p_)

instance Pretty (Located Type) where
    -- | Type pretty printing
    pretty (t :@ _) = case t of
        TId i -> text i
        TVar tv -> text tv
        TTuple ts -> tupled (pretty <$> ts)
        TApp t1 t2 -> pretty t1 <+> parenthesizeIfNeeded t2
        TFun (t1, l) t2 -> parenthesizeIfNeededF t1 <> text "|" <> pretty (LInt l) <> text "|" <+> text "->" <+> parenthesizeIfNeededF t2
      where
        parenthesizeIfNeeded (t :@ p) = case t of
            TApp _ _ -> parens $ pretty (t :@ p)
            TFun _ _ -> parens $ pretty (t :@ p)
            _ -> pretty (t :@ p)
        parenthesizeIfNeededF (t :@ p) = case t of
            TFun _ _ -> parens $ pretty (t :@ p)
            _ -> pretty (t :@ p)