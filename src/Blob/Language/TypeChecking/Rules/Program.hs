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

{-# LANGUAGE TypeApplications, FlexibleContexts, TypeFamilies #-}

module Blob.Language.TypeChecking.Rules.Program where

import Blob.Language.Syntax.Internal.Parsing.Located
import qualified Blob.Language.Syntax.Internal.Desugaring.CoreAST as TP
import Blob.Language.TypeChecking.TypeChecker (Check, TIError)
import Blob.Language.TypeChecking.Internal.Type
import Blob.Language.TypeChecking.Internal.Substitution.Types
import Blob.Language.TypeChecking.Rules.Types.Infer
import Blob.Language.TypeChecking.Internal.Environment
import Blob.Language.TypeChecking.Internal.Constraint
import Blob.Language.TypeChecking.Internal.Errors.BindLack
import Blob.Language.TypeChecking.Internal.Errors.RedefinedVariable
import Blob.Language.TypeChecking.Internal.Errors.RedeclaredVariable
import Blob.Language.TypeChecking.Internal.Errors.MistypedGADT
import Blob.Language.TypeChecking.Internal.Substitution
import Blob.Language.TypeChecking.Solver.TypeHoleSolver
import Blob.Language.TypeChecking.Solver.TypeSolver
import qualified Blob.Language.TypeChecking.Rules.Kinds.Infer as Kind
import qualified Blob.Language.TypeChecking.KindChecker as Kind
import Blob.Language.TypeChecking.Solver.KindSolver (runKindSolver)
import Blob.Language.TypeChecking (runKI)
import Blob.Language.TypeChecking.Internal.Substitution.Kinds (KindSubst)
import Blob.Language.TypeChecking.Internal.Kind
import qualified Data.Map as Map
import qualified Data.Map.Unordered as UMap
import Data.These
import Control.Monad.Except (throwError, liftEither)
import Control.Monad.State (get)
import Control.Monad.Reader (local, ask)
import Control.Monad.Writer (listen, tell)
import Data.Bifunctor (first, second, bimap)
import Control.Applicative (liftA2)
import Control.Monad (void, forM)
import Control.Lens ((%=), (%~))
import Data.Align.Key (alignWithKey)

-- | Infers the definition of a function given a 'GlobalEnv', the name of the function and its value as an 'Expr'.
inferDef :: GlobalEnv -> String -> Located TP.Expr -> Maybe Type -> Check ()
inferDef env name def t1 =
    let res = runTI env $ do
            var <- fresh "#"
            t <- inEnv (name, Scheme [] var) $ infer def
            tell [t :^~: var]
            pure t
    in case res of
        Left err -> throwError err
        Right (t, c) -> case runTypeSolver env ((maybe id (\t2 -> ((t :^~: t2) :)) t1) c) of
            Left err -> throwError err
            Right x -> case runHoleInspect x of
                Left err -> throwError err
                Right _ ->
                    defCtx %= ((TypeEnv $ Map.singleton name (closeOver (apply x t))) `union`)


-- | Separate statements depending on whether they are a function definition or function declaration.
sepStatements :: [TP.Statement] -> Check (UMap.Map String (Located TP.Expr), UMap.Map String (Located TP.Type))
sepStatements = uncurry (liftA2 (,)) . bimap (foldDecls makeRedefinedError mempty) (foldDecls makeRedeclaredError mempty) . separateDecls
  where
    separateDecls []                                 = mempty
    separateDecls (TP.Declaration id' type' : stmts) = second ((id', type'):) (separateDecls stmts)
    separateDecls (TP.Definition id' expr : stmts)   = first ((id', expr):) (separateDecls stmts)
    separateDecls (_ : stmts)                        = separateDecls stmts

    foldDecls :: (String -> TIError) -> UMap.Map String a -> [(String, a)] -> Check (UMap.Map String a)
    foldDecls _ m []              = pure m
    foldDecls err m ((id', t):ts) = case UMap.lookup id' m of
        Nothing -> flip (foldDecls err) ts $ UMap.insert id' t m
        Just _ -> throwError $ err id'

-- | Type checks a function definition, a function declaration (error) or both.
handleStatement :: String -> These (Located TP.Expr) (Located TP.Type) -> Check ()
handleStatement name (This def)      = do
    e <- get
    inferDef e name def Nothing
handleStatement name (That _)        = throwError $ makeBindLackError name
handleStatement name (These def typ) = do
    let ti     = tiType typ
    let gen'ed = closeOver (relax ti)
    GlobalEnv env _ _ _ <- get
    checkKI $ Kind.kiScheme gen'ed

    e <- get
    inferDef e name def (Just ti)

-- | Kind checks a type declaration.
analyseTypeDecl :: String -> CustomScheme -> Check ()
analyseTypeDecl k v@(CustomScheme tvs t) = do
    kind <- checkKI $ do
        args  <- forM tvs (const $ Kind.fresh "k")
        let var = foldr KArr KType args
        k     <- local (_KindEnv %~ Map.insert k var) (Kind.kiCustomScheme v)
        env   <- ask
        cs    <- snd <$> listen (pure ())
        subst <- liftEither $ runKindSolver env ([var :*~: k] <> cs)
        pure $ apply subst var

    schemes <- case t of
        TSum ctors -> do
            let typeDef = foldl (\acc t -> acc `TApp` TVar (TV t)) (TId k) tvs

            void . flip Map.traverseWithKey ctors $ \ctorName (Scheme _ c) -> do
                let (_, r) = unfoldParams c

                e <- get

                case runTypeSolver e [(typeDef :^~: r)] of
                    Left _ -> throwError $ makeGADTWrongReturnTypeError ctorName r typeDef
                    Right _ -> pure ()

            pure ctors
        _          -> pure $ Map.fromList []

    typeDefCtx %= Map.insert k v
    typeDeclCtx . _KindEnv %= Map.insert k kind
    ctorCtx %= (TypeEnv schemes `union`)

-- | type checks a whole 'Program'.
tiProgram :: Located TP.Program -> Check ()
tiProgram (TP.Program stmts :@ _) = do
    remaining <- sepTypeDecls stmts
    stts      <- sepStatements remaining
    sequence_ $ uncurry (alignWithKey handleStatement) stts
  where sepTypeDecls [] = pure []
        sepTypeDecls ((TP.TypeDeclaration k tvs t :@ _):ss) = do
            analyseTypeDecl k (CustomScheme tvs (tiCustomType t))
            sepTypeDecls ss
        sepTypeDecls ((s :@ _):ss) = (s:) <$> sepTypeDecls ss

-------------------------------------------------------------------------

checkKI :: (Substitutable a, Subst a ~ KindSubst) => Kind.KI a -> Check a
checkKI action = do
    GlobalEnv env _ _ _ <- get
    liftEither $ runKI env action