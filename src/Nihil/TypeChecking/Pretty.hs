{-| Pretty printing functions for kinds and types. -}
module Nihil.TypeChecking.Pretty
( -- * Re-exports
  putDoc ) where

import Nihil.TypeChecking.Core
import Nihil.Utils.Source (annotated)
import Text.PrettyPrint.ANSI.Leijen
import qualified Data.Char as Ch (isSymbol)
import qualified Data.Map as Map

instance Pretty Kind where
    pretty KStar                = text "*"
    pretty (KVar v)             = text v
    pretty KArrow               = text "(->)"
    pretty (KApplication k1 k2) = prettyᵏ k1 <+> pretty k2
      where prettyᵏ k@(KApplication KArrow k2) = prettyᵏ k2 <+> text "->"
            prettyᵏ k@KApplication{}           = parens (pretty k)
            prettyᵏ k                          = pretty k

instance Pretty Type' where
    pretty (TId i)              = text i -- if isOperator i then parens (text i) else text i
    pretty (TVar v)             = text v
    pretty (TRigid v)           = text v
    pretty (TTuple ts)          = tupled (fmap pretty ts)
    pretty (TApplication t1 t2) = prettyᵗ (annotated t1) <+> pretty t2
      where prettyᵗ t@(TApplication t1 t2) =
                case annotated t1 of
                    TId i | isOperator i -> prettyᵗ (annotated t2) <+> text i
                    _                    -> parens (pretty t)
            prettyᵗ t                = pretty t
    pretty (TPrim ty)           = text ty
    pretty (TRecord ss ty)      =
        braces (mconcat (punctuate semi (prettyStts ss)) <+> text "|" <+> pretty ty)
      where prettyStts = Map.elems . Map.mapWithKey prettyStt
            prettyStt name ty = text name <+> colon <+> pretty ty

instance Pretty t => Pretty (Scheme t) where
    pretty (Forall vars x) = text "forall" <+> sep (fmap text vars) <> dot <+> pretty x

isOperator :: String -> Bool
isOperator (x:_) = Ch.isSymbol x || isMultiSymbol x
  where isMultiSymbol '!' = True
        isMultiSymbol '#' = True
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
