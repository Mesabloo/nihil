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

module Blob.Language.Syntax.Rules.Desugaring.Types.Type where

import Blob.Language.Syntax.Internal.Parsing.Located
import qualified Blob.Language.Syntax.Internal.Parsing.AST as P
import qualified Blob.Language.Syntax.Internal.Desugaring.CoreAST as D
import Blob.Language.Syntax.Desugarer (Desugarer)
import Blob.Language.Syntax.Internal.Lexing.SourceSpan (SourceSpan(..))
import Control.Lens ((^.))
import Data.Foldable (foldlM)

-- | Desugars a 'P.Type' into a 'D.Type'.
desugarType :: String -> Located P.Type -> Desugarer (Located D.Type)
desugarType _ (P.TId name :@ p) = pure (D.TId name :@ p)
desugarType _ (P.TApp [] :@ _) = undefined -- ! never happening
desugarType fileName (P.TApp (t:ts) :@ p) = do
    t1 <- desugarType fileName t

    foldlM (\acc t' -> do
        t2 <- desugarType fileName t'
        let Just (SourceSpan beg _) = acc ^. pos
            Just (SourceSpan _ end) = t2  ^. pos

        pure (D.TApp acc t2 :@ Just (SourceSpan beg end)) ) t1 ts
desugarType _ (P.TVar name :@ p) = pure (D.TVar name :@ p)
desugarType fileName (P.TFun (t1, l) t2 :@ p) = do
    t1' <- desugarType fileName t1
    t2' <- desugarType fileName t2
    pure (D.TFun (t1', l) t2' :@ p)
desugarType fileName (P.TTuple ts :@ p) = do
    ts' <- mapM (desugarType fileName) ts

    pure (D.TTuple ts' :@ p)
desugarType fileName (P.TList ts :@ p) =
    flip (:@) p . (^. located) <$> foldlM (\acc t -> do
        t' <- desugarType fileName t

        pure (D.TApp acc t' :@ Nothing) ) (D.TId "[]" :@ Nothing) ts
