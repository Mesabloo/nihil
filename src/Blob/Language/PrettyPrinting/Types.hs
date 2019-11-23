{-# LANGUAGE FlexibleContexts, FlexibleInstances, LambdaCase #-}

module Blob.Language.PrettyPrinting.Types where

import Blob.Language.TypeChecking.Internal.Type
import Text.PrettyPrint.Leijen hiding ((<$>))
import Data.List (intersperse)

instance Pretty Type where
    -- | Type pretty printing
    pretty = \case
        TVar (TV i) -> text i
        TRigid (TV i) -> text i
        TId i -> text i
        TInt -> text "Integer"
        TFloat -> text "Double"
        TChar -> text "Char"
        TTuple ts -> parens . mconcat $ intersperse (text ", ") (pretty <$> ts)
        TFun (t1, l) t2 -> parenthesizeIfNeeded t1 <> text "|" <> text (show l) <> text "|" <+> text "->" <+> pretty t2
        TApp t1 t2 -> pretty t1 <+> parenthesizeIfNeeded t2
      where parenthesizeIfNeeded t = case t of
                TFun _ _ -> parens $ pretty t
                TApp _ _ -> parens $ pretty t
                _ -> pretty t