{-| Pretty printing functions for the abstract core language. It should print a fairly modified version of what the user wrote. -}
module Nihil.Syntax.Pretty.Abstract where

import Nihil.Syntax.Abstract.Core
import Nihil.Utils.Source
import Text.PrettyPrint.ANSI.Leijen
import Prelude hiding ((<$>))
import qualified Data.Map as Map
import qualified Data.Char as Ch (isSymbol)

indent' :: Int
indent' = 4

-- | Aligns two documents on the same column.
($$) :: Doc -> Doc -> Doc
($$) x y = align (x <$> y)

instance Pretty Program where
    pretty (Program [])   = linebreak
    pretty (Program stts) = nest indent' (foldl1 ($$) (fmap pretty stts)) <> linebreak

instance Pretty Statement' where
    pretty (FunctionDeclaration name ty) = text name <+> colon </> pretty ty
    pretty (FunctionDefinition name ex)  = text name <+> equals </> pretty ex
    pretty (TypeDefinition name tvs ct)  = case annotated ct of
        TypeAlias ty  -> text "type" <+> text name <+> sep (fmap text tvs) <+> equals <+> pretty ty
        SumType ctors -> text "data" <+> text name <+> sep (fmap text tvs) <+> text "where" <+> semiBraces (Map.elems (Map.mapWithKey pretty' ctors))
          where pretty' name scheme = text name <+> colon <+> pretty scheme <+> semi
    pretty (ClassDefinition ty decls) =
        text "class" <+> pretty ty <+> text "where" <+> semiBraces (fmap ((<+> semi) . pretty) decls)
    pretty (InstanceDefinition ty defs) =
        text "instance" <+> pretty ty <+> text "where" <+> semiBraces (fmap ((<+> semi) . pretty) defs)

instance Pretty Type' where
    pretty (TId i)              = if isOperator i then parens (text i) else text i
    pretty (TTuple ts)          = tupled (fmap pretty ts)
    pretty (TVar v)             = text v
    pretty (TApplication t1 t2) = pretty t1 <+> prettyᵗ (annotated t2)
      where prettyᵗ :: Type' -> Doc
            prettyᵗ t@TApplication{} = parens (pretty t)
            prettyᵗ t                = pretty t

instance Pretty Expr' where
    pretty (EId i)                = if isOperator i then parens (text i) else text i
    pretty (ELiteral lit)         = pretty lit
    pretty (ELambda pat ex)       = backslash <+> pretty pat <+> text "->" </> pretty ex
    pretty (EApplication e1 e2)   = pretty e1 <+> prettyᵉ (annotated e2)
      where prettyᵉ :: Expr' -> Doc
            prettyᵉ e@EApplication{} = parens (pretty e)
            prettyᵉ e@ELambda{}      = parens (pretty e)
            prettyᵉ e@EMatch{}       = parens (pretty e)
            prettyᵉ e@ELet{}         = parens (pretty e)
            prettyᵉ e                = pretty e
    pretty (ETuple es)            = tupled (fmap pretty es)
    pretty ETypeHole              = text "_"
    pretty (ETypeAnnotated ex ty) = parens (pretty ex <+> colon <+> pretty ty)
    pretty (EMatch e1 branches)   = nest indent' (text "match" <+> pretty e1 <+> text "with" <$> prettyBranches branches)
      where prettyBranches = foldl1 ($$) . fmap f
            f (pat, expr)  = pretty pat <+> text "->" <+> pretty expr
    pretty (ELet stts expr)       = text "let" <+> pretty (Program stts) </> text "in" <+> pretty expr

instance Pretty Pattern' where
    pretty PWildcard               = text "_"
    pretty (PId i)                 = if isOperator i then parens (text i) else text i
    pretty (PLiteral lit)          = pretty lit
    pretty (PTypeAnnotated pat ty) = parens (pretty pat <+> colon <+> pretty ty)
    pretty (PConstructor name ps)  = text name <+> sep (fmap pretty ps)
    pretty (PTuple ps)             = tupled (fmap pretty ps)

instance Pretty Literal where
    pretty (LInteger i)   = integer i
    pretty (LCharacter c) = text (show c)
    pretty (LFloat d)     = double d

instance Pretty Scheme where
    pretty (Forall tvs ty) = text "forall" <+> sep (fmap text tvs) <+> dot <+> pretty ty

isOperator :: String -> Bool
isOperator (x:_) = Ch.isSymbol x || isMultiSymbol x
  where isMultiSymbol '!' = True
        isMultiSymbol '$' = True
        isMultiSymbol '%' = True
        isMultiSymbol '&' = True
        isMultiSymbol '.' = True
        isMultiSymbol '<' = True
        isMultiSymbol '=' = True
        isMultiSymbol '>' = True
        isMultiSymbol '?' = True
        isMultiSymbol '^' = True
        isMultiSymbol '~' = True
        isMultiSymbol '|' = True
        isMultiSymbol '@' = True
        isMultiSymbol '*' = True
        isMultiSymbol '/' = True
        isMultiSymbol '-' = True
        isMultiSymbol '+' = True
        isMultiSymbol ':' = True
        isMultiSymbol  _  = False
isOperator _ = True -- I don't care, it won't happen.