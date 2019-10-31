{-# LANGUAGE LambdaCase #-}

-- | This module holds generator functions for some data-types, using Template Haskell.
module Blob.Interpreter.JIT.TH where

import Language.Haskell.TH
import Control.Monad
import Data.Char
import Data.Maybe

-- | Makes functions for each register in the 'Reg' data-type.
--
-- A function is of this form:
--
-- > lowerReg = Reg reg
--
-- where @reg@ is the current register, and @lowerRed@ is the lowercase name of the register.
mkRegFuns :: DecsQ
mkRegFuns = do
    Just nm <- lookupTypeName "Reg"
    (TyConI (DataD _ _ _ _ cons _)) <- reify nm

    decs <- forM cons $ \case
        NormalC name _ -> do
            let lower = mkName (toLower <$> nameBase name)
            pure (Just $ genFun lower name)
        _ -> pure Nothing
    pure (catMaybes decs)
  where genFun low n = ValD (VarP low) (NormalB $ ConE (mkName "Reg") `AppE` ConE n) []