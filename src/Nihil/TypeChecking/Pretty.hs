{-| Pretty printing functions for kinds and types. -}
module Nihil.TypeChecking.Pretty where

import Nihil.TypeChecking.Core
import Nihil.Utils.Source (annotated)
import Text.PrettyPrint.ANSI.Leijen

instance Pretty Kind where
    pretty KStar                = text "*"
    pretty (KVar v)             = text v
    pretty KArrow               = text "(->)"
    pretty (KApplication k1 k2) =
        pretty k1 <+> prettyᵏ k2
      where prettyᵏ k@KApplication{} = parens (pretty k)
            prettyᵏ k                = pretty k

instance Pretty Type' where
    pretty (TId i)              = text i
    pretty (TVar v)             = text v
    pretty (TRigid v)           = text v
    pretty (TTuple ts)          = tupled (fmap pretty ts)
    pretty (TApplication t1 t2) = pretty t1 <+> prettyᵗ (annotated t2)
      where prettyᵗ t@TApplication{} = parens (pretty t)
            prettyᵗ t                = pretty t

instance Pretty t => Pretty (Scheme t) where
    pretty (Forall vars x) = text "forall" <+> sep (fmap text vars) <> dot <+> pretty x