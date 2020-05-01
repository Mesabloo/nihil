{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

{-| Pretty printing functions for the concrete core language. It should print quite faithfully to what the user wrote. -}
module Nihil.Syntax.Pretty.Concrete
( pretty, Pretty ) where

import Nihil.Syntax.Concrete.Core
import Nihil.Utils.Source (annotated)
import Nihil.Syntax.Concrete.Lexer (TokenClass(..))
import Text.PrettyPrint.ANSI.Leijen
import Prelude hiding ((<$>))
import Control.Arrow ((>>>))
import Data.List (intersperse)
import qualified Data.Map as Map

indent' :: Int
indent' = 4

-- | Aligns two documents on the same column.
($$) :: Doc -> Doc -> Doc
($$) x y = align (x <$> y)

instance Pretty TokenClass where
    pretty TkMatch                = text "keyword 'match'"
    pretty TkWith                 = text "keyword 'with'"
    pretty TkData                 = text "keyword 'data'"
    pretty TkType                 = text "keyword 'type'"
    pretty TkLet                  = text "keyword 'let'"
    pretty TkIn                   = text "keyword 'in'"
    pretty TkWhere                = text "keyword 'where'"
    pretty TkInfixL               = text "keyword 'infixl'"
    pretty TkInfixR               = text "keyword 'infixr'"
    pretty TkEquals               = text "symbol '='"
    pretty TkColon                = text "symbol ':'"
    pretty TkSemi                 = text "symbol ';'"
    pretty TkBackslash            = text "symbol '\\'"
    pretty TkArrow                = text "symbol '->'"
    pretty TkImplies              = text "symbol '=>'"
    pretty TkBacktick             = text "symbol '`'"
    pretty TkLambda               = text "symbol 'λ'"
    pretty TkUnderscore           = text "symbol '_'"
    pretty (TkInt i)              = text "integer " <> squotes (integer i)
    pretty (TkFloat d)            = text "float " <> squotes (double d)
    pretty (TkString s)           = text "string " <> string s
    pretty (TkChar c)             = text "character " <> char c
    pretty (TkLIdent i)           = text "identifier " <> squotes (text i)
    pretty (TkUIdent i)           = text "identifier " <> squotes (text i)
    pretty (TkSym s)              = text "symbol " <> squotes (text s)
    pretty TkLParen               = text "symbol '('"
    pretty TkRParen               = text "symbol ')'"
    pretty TkComma                = text "symbol ','"
    pretty TkBar                  = text "symbol '|'"
    pretty (TkInlineComment c)    = text "-- " <> text c
    pretty (TkMultilineComment c) = text "{- " <> text c <> text "-}"
    pretty TkEOF                  = text "<EOF>"
    pretty TkEOL                  = text "<newline>"

instance Show TokenClass where
    show = show . pretty

instance Pretty Program where
    pretty (Program [])   = linebreak
    pretty (Program stts) = nest indent' (foldl1 ($$) (fmap pretty stts)) <> linebreak

instance Pretty Statement where
    pretty (FunDeclaration name ty)     = nest indent' (text name <+> colon </> pretty ty)
    pretty (FunDefinition name args ex) = nest indent' (text name <+> sep (fmap pretty args) <+> equals </> pretty ex)
    pretty (OperatorFixity name fixity) = text "infix" <> associativity fixity <+> precedence fixity <+> text name
      where associativity = annotated >>> \case
                Infix L _ -> text "l"
                Infix R _ -> text "r"
            precedence    = annotated >>> \case
                Infix _ p -> integer p
    pretty (TypeDefinition name tvs ct) = case annotated ct of
        TypeAlias ty  ->
            text "type" <+> text name <> sep (fmap text tvs) <+> equals <+> pretty ty
        SumType ctors ->
            text "data" <+> text name <> sep (fmap text tvs) <+> equals
             <+> sep (intersperse (text "|") (Map.elems (Map.mapWithKey prettyCtor ctors)))
          where prettyCtor :: String -> [AType] -> Doc
                prettyCtor name ts = text name <+> sep (fmap pretty ts)
        GADT ctors    ->
            text "data" <+> text name <+> sep (fmap text tvs) <+> text "where"
             <+> semiBraces (Map.elems (Map.mapWithKey prettyCtor ctors))
          where prettyCtor :: String -> [AType] -> Doc
                prettyCtor name ts = text name <+> colon <+> pretty ts

instance {-# OVERLAPPING #-} Pretty [AType] where
    pretty = sep . fmap pretty

instance Pretty Type where
    pretty (TOperator name)  = text name
    pretty (TId i)           = text i
    pretty (TVar v)          = text v
    pretty (TParens p)       = parens (pretty p)
    pretty (TTuple t)        = tupled (fmap pretty t)
    pretty (TApplication ts) = sep (fmap pretty ts)

instance {-# OVERLAPPING #-} Pretty [AAtom] where
    pretty = sep . fmap pretty

instance Pretty Pattern where
    pretty (POperator name)       = text name
    pretty (PId i)                = text i
    pretty PWildcard              = text "_"
    pretty (PParens p)            = parens (pretty p)
    pretty (PTypeAnnotated p t)   = pretty p <+> colon <+> pretty t
    pretty (PLiteral lit)         = pretty lit
    pretty (PConstructor name ps) = text name <+> sep (fmap pretty ps)
    pretty (PTuple p)             = tupled (fmap pretty p)

instance Pretty Atom where
    pretty (AOperator name)     = text name
    pretty (ALiteral lit)       = pretty lit
    pretty (AId i)              = text i
    pretty ATypeHole            = text "_"
    pretty (AParens e)          = parens (pretty e)
    pretty (ALambda ps e)       = backslash <+> sep (fmap pretty ps) <+> text "->" <+> pretty e
    pretty (ATypeAnnotated e t) = pretty e <+> colon <+> pretty t
    pretty (AApplication as)    = sep (fmap pretty as)
    pretty (ATuple as)          = tupled (fmap pretty as)
    pretty (ALet stts e)        = text "let" <+> pretty (Program stts) <+> text "in" <+> pretty e
    pretty (AWhere e stts)      = pretty e <$> text "where" <$> indent indent' (pretty (Program stts))
    pretty (AMatch e1 branches) = nest indent' (text "match" <+> pretty e1 <+> text "with" <$> prettyBranches branches)
      where prettyBranches  = foldl1 ($$) . fmap f
            f (pat, ex)     = sep (fmap pretty pat) <+> text "->" <+> pretty ex

instance Pretty Literal where
    pretty (LString s)    = text (show s)
    pretty (LCharacter c) = text (show c)
    pretty (LDouble d)    = double d
    pretty (LInteger i)   = integer i
