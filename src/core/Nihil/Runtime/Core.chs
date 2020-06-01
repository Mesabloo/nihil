{-# LANGUAGE ForeignFunctionInterface #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module Nihil.Runtime.Core where

import Data.Int
import Data.Char (chr, ord)
import Foreign.Storable (Storable(..))
import Foreign.Ptr
import Foreign.C
import Foreign.Marshal.Array
import Foreign.Marshal.Alloc (malloc, free)

genToEnum :: (Integral a, Enum b) => a -> b
genToEnum = toEnum . fromIntegral

genFromEnum :: (Integral a, Enum b) => b -> a
genFromEnum = fromIntegral . fromEnum

---------------------------------------------------------------------------

#include "Nihil/Runtime/core.h"

{#enum VPattern_Cons as VPatternCons {}#}

data C_VPattern
    = C_PInteger Int64
    | C_PDouble Double
    | C_PCharacter Char
    | C_PId CString
    | C_PWildcard
    | C_PTuple CULong (Ptr (Ptr C_VPattern))
    | C_PConstructor CString CULong (Ptr (Ptr C_VPattern))

data VPattern
    = PInteger { p_i :: Integer }
    | PDouble { p_d :: Double }
    | PCharacter { p_c :: Char }
    | PId { p_name :: String }
    | PWildcard
    | PTuple { p_patterns :: [VPattern] }
    | PConstructor { p_name :: String, p_args :: [VPattern] }
  deriving (Show, Eq, Ord)

instance Storable C_VPattern where
    sizeOf _ = {#sizeof VPattern_s#}
    alignment _ = {#alignof VPattern_s#}

    peek ptr = do
        ty <- genToEnum <$> {#get struct VPattern_s->ctor#} ptr
        case ty of
            CrPInteger     -> C_PInteger . fromIntegral
                <$> {#get struct VPattern_s->value.pInteger.p_i#} ptr
            CrPDouble      -> C_PDouble . (\(CDouble x) -> x)
                <$> {#get struct VPattern_s->value.pDouble.p_d#} ptr
            CrPCharacter   -> C_PCharacter . (\(CChar x) -> chr (fromIntegral x))
                <$> {#get struct VPattern_s->value.pCharacter.p_c#} ptr
            CrPId          -> C_PId . castPtr
                <$> {#get struct VPattern_s->value.pId.p_name#} ptr
            CrPWildcard    -> pure C_PWildcard
            CrPTuple       -> C_PTuple
                <$> {#get struct VPattern_s->value.pTuple.n#} ptr
                <*> (castPtr <$> {#get struct VPattern_s->value.pTuple.p_patterns#} ptr)
            CrPConstructor -> C_PConstructor
                <$> (castPtr <$> {#get struct VPattern_s->value.pConstructor.p_name#} ptr)
                <*> {#get struct VPattern_s->value.pTuple.n#} ptr
                <*> (castPtr <$> {#get struct VPattern_s->value.pConstructor.p_args#} ptr)

    poke ptr (C_PInteger i)               = do
        {#set struct VPattern_s->ctor#} ptr (genFromEnum CrPInteger)
        {#set struct VPattern_s->value.pInteger.p_i#} ptr (CLong i)
    poke ptr (C_PDouble d)                = do
        {#set struct VPattern_s->ctor#} ptr (genFromEnum CrPDouble)
        {#set struct VPattern_s->value.pDouble.p_d#} ptr (CDouble d)
    poke ptr (C_PCharacter c)             = do
        {#set struct VPattern_s->ctor#} ptr (genFromEnum CrPCharacter)
        {#set struct VPattern_s->value.pCharacter.p_c#} ptr (CChar (fromIntegral $ ord c))
    poke ptr (C_PId name)                 = do
        {#set struct VPattern_s->ctor#} ptr (genFromEnum CrPId)
        {#set struct VPattern_s->value.pId.p_name#} ptr (castPtr name)
    poke ptr C_PWildcard                  = do
        {#set struct VPattern_s->ctor#} ptr (genFromEnum CrPWildcard)
    poke ptr (C_PTuple n ps)              = do
        {#set struct VPattern_s->ctor#} ptr (genFromEnum CrPTuple)
        {#set struct VPattern_s->value.pTuple.n#} ptr n
        {#set struct VPattern_s->value.pTuple.p_patterns#} ptr (castPtr ps)
    poke ptr (C_PConstructor name n args) = do
        {#set struct VPattern_s->ctor#} ptr (genFromEnum CrPConstructor)
        {#set struct VPattern_s->value.pConstructor.p_name#} ptr (castPtr name)
        {#set struct VPattern_s->value.pTuple.n#} ptr n
        {#set struct VPattern_s->value.pConstructor.p_args#} ptr (castPtr args)

peekPattern :: Ptr VPattern -> IO VPattern
peekPattern ptr =
    peek (castPtr ptr) >>= \case
        C_PInteger i               -> pure $ PInteger (fromIntegral i)
        C_PDouble d                -> pure $ PDouble d
        C_PCharacter c             -> pure $ PCharacter c
        C_PId name                 -> PId <$> peekCString name
        C_PWildcard                -> pure PWildcard
        C_PTuple n pats            -> do
            arr <- peekArray (fromIntegral n) pats
            PTuple <$> traverse (peekPattern . castPtr) arr
        C_PConstructor name n pats -> do
            arr <- peekArray (fromIntegral n) pats
            PConstructor <$> peekCString name
                         <*> traverse (peekPattern . castPtr) arr

newPattern :: VPattern -> IO (Ptr VPattern)
newPattern pat = do
    p <- malloc
    case pat of
        PInteger i             -> poke p (C_PInteger (fromIntegral i))
        PDouble d              -> poke p (C_PDouble d)
        PCharacter c           -> poke p (C_PCharacter c)
        PId name               -> poke p . C_PId =<< newCString name
        PWildcard              -> poke p C_PWildcard
        PTuple pats            -> do
            arr <- newArray =<< traverse (fmap castPtr . newPattern) pats
            poke p (C_PTuple (fromIntegral $ length pats) arr)
        PConstructor name pats -> do
            arr <- newArray =<< traverse (fmap castPtr . newPattern) pats
            poke p . (\ s -> C_PConstructor s (fromIntegral $ length pats) arr) =<< newCString name
    pure (castPtr p)

freePattern :: Ptr VPattern -> IO ()
freePattern ptr = do
    pat <- peek (castPtr ptr)
    case pat of
        C_PTuple n pats            -> do
            pat <- peekArray (fromIntegral n) (castPtr pats)
            mapM_ freePattern pat
            free pats
        C_PId name                 -> do
            free name
        C_PConstructor name n pats -> do
            free name
            pat <- peekArray (fromIntegral n) (castPtr pats)
            mapM_ freePattern pat
            free pats
        _                          -> pure ()
    free ptr

------------------------------------------------------------------

{#enum VExpr_Cons as VExprCons {}#}

data C_VBranch
    = C_VBranch
    { b_pattern :: Ptr C_VPattern
    , b_expr :: Ptr C_VExpr }

data C_VDecl
    = C_VDecl
    { d_name :: CString
    , d_val :: Ptr C_VExpr }

data C_VExpr
    = C_EId CString
    | C_EInteger Int64
    | C_EDouble Double
    | C_ECharacter Char
    | C_ELambda (Ptr C_VPattern) (Ptr C_VExpr)
    | C_EApplication (Ptr C_VExpr) (Ptr C_VExpr)
    | C_ETuple CULong (Ptr (Ptr C_VExpr))
    | C_ETypeHole
    | C_EMatch (Ptr C_VExpr) CULong (Ptr (Ptr C_VBranch))
    | C_ELet CULong (Ptr (Ptr C_VDecl)) (Ptr C_VExpr)
    | C_ERecord CULong (Ptr (Ptr C_VDecl))

data VExpr
    = EId { v_name :: String }
    | EInteger { v_i :: Integer }
    | EDouble { v_d :: Double }
    | ECharacter { v_c :: Char }
    | ELambda { v_arg :: VPattern, v_ex :: VExpr }
    | EApplication { v_fun :: VExpr, v_x :: VExpr }
    | ETuple { v_vals :: [VExpr] }
    | ETypeHole
    | EMatch { v_expr :: VExpr, v_branches :: [(VPattern, VExpr)] }
    | ELet { v_decls :: [(String, VExpr)], v_expr :: VExpr }
    | ERecord { v_fields :: [(String, VExpr)] }
  deriving (Show, Eq, Ord)

instance Storable C_VBranch where
    sizeOf _ = {#sizeof VBranch_s#}
    alignment _ = {#alignof VBranch_s#}

    peek ptr =
        C_VBranch <$> (castPtr <$> {#get struct VBranch_s->b_pattern#} ptr)
                  <*> (castPtr <$> {#get struct VBranch_s->b_expr#} ptr)

    poke ptr (C_VBranch p e) = do
        {#set struct VBranch_s->b_pattern#} ptr (castPtr p)
        {#set struct VBranch_s->b_expr#} ptr (castPtr e)

instance Storable C_VDecl where
    sizeOf _ = {#sizeof VDecl_s#}
    alignment _ ={#alignof VDecl_s#}

    peek ptr =
        C_VDecl <$> (castPtr <$> {#get struct VDecl_s->d_name#} ptr)
                <*> (castPtr <$> {#get struct VDecl_s->d_val#} ptr)

    poke ptr (C_VDecl n v) = do
        {#set struct VDecl_s->d_name#} ptr (castPtr n)
        {#set struct VDecl_s->d_val#} ptr (castPtr v)

instance Storable C_VExpr where
    sizeOf _ = {#sizeof VExpr_s#}
    alignment _ = {#alignof VExpr_s#}

    peek ptr = do
        ty <- genToEnum <$> {#get struct VExpr_s->ctor#} ptr
        case ty of
            CrEId          -> C_EId <$> {#get struct VExpr_s->value.eId.v_name#} ptr
            CrEInteger     -> C_EInteger . fromIntegral
                <$> {#get struct VExpr_s->value.eInteger.v_i#} ptr
            CrEDouble      -> C_EDouble . (\(CDouble x) -> x)
                <$> {#get struct VExpr_s->value.eDouble.v_d#} ptr
            CrECharacter   -> C_ECharacter . (\(CChar c) -> chr (fromIntegral c))
                <$> {#get struct VExpr_s->value.eCharacter.v_c#} ptr
            CrELambda      -> C_ELambda
                <$> (castPtr <$> {#get struct VExpr_s->value.eLambda.v_arg#} ptr)
                <*> (castPtr <$> {#get struct VExpr_s->value.eLambda.v_ex#} ptr)
            CrEApplication -> C_EApplication
                <$> (castPtr <$> {#get struct VExpr_s->value.eApplication.v_fun#} ptr)
                <*> (castPtr <$> {#get struct VExpr_s->value.eApplication.v_x#} ptr)
            CrETuple       -> C_ETuple
                <$> {#get struct VExpr_s->value.eTuple.n#} ptr
                <*> (castPtr <$> {#get struct VExpr_s->value.eTuple.v_vals#} ptr)
            CrETypeHole    -> pure C_ETypeHole
            CrEMatch       -> C_EMatch
                <$> (castPtr <$> {#get struct VExpr_s->value.eMatch.v_expr#} ptr)
                <*> {#get struct VExpr_s->value.eMatch.n#} ptr
                <*> (castPtr <$> {#get struct VExpr_s->value.eMatch.v_branches#} ptr)
            CrELet         -> C_ELet
                <$> {#get struct VExpr_s->value.eLet.n#} ptr
                <*> (castPtr <$> {#get struct VExpr_s->value.eLet.v_decls#} ptr)
                <*> (castPtr <$> {#get struct VExpr_s->value.eLet.v_expr#} ptr)
            CrERecord      -> C_ERecord
                <$> {#get struct VExpr_s->value.eRecord.n#} ptr
                <*> (castPtr <$> {#get struct VExpr_s->value.eRecord.v_fields#} ptr)

    poke ptr (C_EId name)             = do
        {#set struct VExpr_s->ctor#} ptr (genFromEnum CrEId)
        {#set struct VExpr_s->value.eId.v_name#} ptr (castPtr name)
    poke ptr (C_EInteger i)           = do
        {#set struct VExpr_s->ctor#} ptr (genFromEnum CrEInteger)
        {#set struct VExpr_s->value.eInteger.v_i#} ptr (CLong i)
    poke ptr (C_EDouble d)            = do
        {#set struct VExpr_s->ctor#} ptr (genFromEnum CrEDouble)
        {#set struct VExpr_s->value.eDouble.v_d#} ptr (CDouble d)
    poke ptr (C_ECharacter c)         = do
        {#set struct VExpr_s->ctor#} ptr (genFromEnum CrECharacter)
        {#set struct VExpr_s->value.eCharacter.v_c#} ptr (CChar (fromIntegral $ ord c))
    poke ptr (C_ELambda pat ex)       = do
        {#set struct VExpr_s->ctor#} ptr (genFromEnum CrELambda)
        {#set struct VExpr_s->value.eLambda.v_arg#} ptr (castPtr pat)
        {#set struct VExpr_s->value.eLambda.v_ex#} ptr (castPtr ex)
    poke ptr (C_EApplication fun arg) = do
        {#set struct VExpr_s->ctor#} ptr (genFromEnum CrEApplication)
        {#set struct VExpr_s->value.eApplication.v_fun#} ptr (castPtr fun)
        {#set struct VExpr_s->value.eApplication.v_x#} ptr (castPtr arg)
    poke ptr (C_ETuple n vals)         = do
        {#set struct VExpr_s->ctor#} ptr (genFromEnum CrETuple)
        {#set struct VExpr_s->value.eTuple.n#} ptr n
        {#set struct VExpr_s->value.eTuple.v_vals#} ptr (castPtr vals)
    poke ptr C_ETypeHole              = do
        {#set struct VExpr_s->ctor#} ptr (genFromEnum CrETypeHole)
    poke ptr (C_EMatch ex n brs)      = do
        {#set struct VExpr_s->ctor#} ptr (genFromEnum CrEMatch)
        {#set struct VExpr_s->value.eMatch.v_expr#} ptr (castPtr ex)
        {#set struct VExpr_s->value.eMatch.n#} ptr n
        {#set struct VExpr_s->value.eMatch.v_branches#} ptr (castPtr brs)
    poke ptr (C_ELet n decls ex)      = do
        {#set struct VExpr_s->ctor#} ptr (genFromEnum CrELet)
        {#set struct VExpr_s->value.eLet.n#} ptr n
        {#set struct VExpr_s->value.eLet.v_decls#} ptr (castPtr decls)
        {#set struct VExpr_s->value.eLet.v_expr#} ptr (castPtr ex)
    poke ptr (C_ERecord n fields)     = do
        {#set struct VExpr_s->ctor#} ptr (genFromEnum CrERecord)
        {#set struct VExpr_s->value.eRecord.n#} ptr n
        {#set struct VExpr_s->value.eRecord.v_fields#} ptr (castPtr fields)

peekExpr :: Ptr VExpr -> IO VExpr
peekExpr ptr =
    peek (castPtr ptr) >>= \case
        C_EInteger i              -> pure (EInteger $ fromIntegral i)
        C_EDouble d               -> pure (EDouble d)
        C_ECharacter c            -> pure (ECharacter c)
        C_EId name                -> EId <$> peekCString name
        C_ELambda arg ex          -> ELambda
            <$> peekPattern (castPtr arg)
            <*> peekExpr (castPtr ex)
        C_EApplication fun arg    -> EApplication
            <$> peekExpr (castPtr fun)
            <*> peekExpr (castPtr arg)
        C_ETuple n exs            -> do
            arr <- peekArray (fromIntegral n) exs
            ETuple <$> traverse (peekExpr . castPtr) arr
        C_ETypeHole               -> pure ETypeHole
        C_EMatch ex n brs         -> do
            arr <- peekArray (fromIntegral n) brs
            EMatch <$> peekExpr (castPtr ex)
                   <*> traverse (unBranch . castPtr) arr
        C_ELet n decls ex         -> do
            arr <- peekArray (fromIntegral n) decls
            ELet <$> traverse (unDecl . castPtr) arr
                 <*> peekExpr (castPtr ex)
        C_ERecord n fields        -> do
            arr <- peekArray (fromIntegral n) fields
            ERecord <$> traverse (unDecl . castPtr) arr
  where unBranch ptr =
            peek (castPtr ptr) >>= \ (C_VBranch pat ex) -> (,)
                <$> peekPattern (castPtr pat)
                <*> peekExpr (castPtr ex)
        unDecl ptr =
            peek (castPtr ptr) >>= \ (C_VDecl name ex) -> (,)
                <$> peekCString name
                <*> peekExpr (castPtr ex)

newExpr :: VExpr -> IO (Ptr VExpr)
newExpr ex = do
    p <- malloc
    case ex of
        EInteger i           -> poke p (C_EInteger (fromIntegral i))
        EDouble d            -> poke p (C_EDouble d)
        ECharacter c         -> poke p (C_ECharacter c)
        EId name             -> poke p . C_EId =<< newCString name
        ELambda pat ex       -> do
            pa <- castPtr <$> newPattern pat
            e <- castPtr <$> newExpr ex
            poke p (C_ELambda pa e)
        EApplication fun arg -> do
            f <- castPtr <$> newExpr fun
            a <- castPtr <$> newExpr arg
            poke p (C_EApplication f a)
        ETuple es            -> do
            arr <- newArray =<< traverse (fmap castPtr . newExpr) es
            poke p (C_ETuple (fromIntegral $ length es) arr)
        ETypeHole            -> poke p C_ETypeHole
        EMatch ex brs        -> do
            arr <- newArray =<< traverse branch brs
            poke p . (\ e -> C_EMatch e (fromIntegral $ length brs) arr) =<< fmap castPtr (newExpr ex)
        ELet decls ex        -> do
            arr <- newArray =<< traverse decl decls
            poke p . C_ELet (fromIntegral $ length decls) arr =<< fmap castPtr (newExpr ex)
        ERecord fields       -> do
            arr <- newArray =<< traverse decl fields
            poke p $ C_ERecord (fromIntegral $ length fields) arr
    pure (castPtr p)
  where branch (pat, ex) = do
            p <- malloc
            poke p =<< C_VBranch <$> (castPtr <$> newPattern pat)
                                 <*> (castPtr <$> newExpr ex)
            pure (castPtr p)
        decl (name, ex) = do
            p <- malloc
            poke p =<< C_VDecl <$> newCString name
                               <*> (castPtr <$> newExpr ex)
            pure (castPtr p)

freeExpr :: Ptr VExpr -> IO ()
freeExpr ptr = do
    ex <- peek (castPtr ptr)
    case ex of
        C_EId name             -> free name
        C_ELambda pat ex       -> do
            freePattern (castPtr pat)
            freeExpr (castPtr ex)
        C_EApplication fun arg -> do
            freeExpr (castPtr fun)
            freeExpr (castPtr arg)
        C_ETuple n es          -> do
            exs <- peekArray (fromIntegral n) es
            mapM_ (freeExpr . castPtr) exs
            free es
        C_EMatch ex n brs      -> do
            freeExpr (castPtr ex)
            branches <- peekArray (fromIntegral n) brs
            mapM_ (freeBranch . castPtr) branches
            free brs
        C_ELet n decls ex      -> do
            ds <- peekArray (fromIntegral n) decls
            mapM_ (freeDecl . castPtr) ds
            freeExpr (castPtr ex)
            free decls
        C_ERecord n fields     -> do
            fs <- peekArray (fromIntegral n) fields
            mapM_ (freeDecl . castPtr) fs
            free fields
        _ -> pure ()
    free ptr
  where freeBranch ptr = do
            C_VBranch pat ex <- peek (castPtr ptr)
            freePattern (castPtr pat)
            freeExpr (castPtr ex)
            free ptr
        freeDecl ptr = do
            C_VDecl name ex <- peek (castPtr ptr)
            free name
            freeExpr (castPtr ex)
            free ptr

data C_Binding
    = C_Binding CString (Ptr C_VExpr)

instance Storable C_Binding where
    sizeOf _ = {#sizeof Binding_s#}
    alignment _ = {#alignof Binding_s#}
    peek ptr = C_Binding
        <$> {#get struct Binding_s->b_name#} ptr
        <*> (castPtr <$> {#get struct Binding_s->b_val#} ptr)
    poke ptr (C_Binding name ex) = do
        {#set struct Binding_s->b_name#} ptr name
        {#set struct Binding_s->b_val#} ptr (castPtr ex)

newBinding :: (String, VExpr) -> IO (Ptr C_Binding)
newBinding (name, def) = do
    p <- malloc
    poke p =<<
        C_Binding <$> newCString name
                  <*> (castPtr <$> newExpr def)
    pure (castPtr p)

peekBinding :: Ptr C_Binding -> IO (String, VExpr)
peekBinding ptr = do
    C_Binding name ex <- peek (castPtr ptr)
    (,) <$> peekCString name
        <*> peekExpr (castPtr ex)

freeBinding :: Ptr C_Binding -> IO ()
freeBinding ptr = do
    C_Binding name ex <- peek (castPtr ptr)
    free name
    freeExpr (castPtr ex)
    free ptr
