module Blob.KindChecking.Types where

import Text.PrettyPrint.Leijen (Doc)
import Control.Monad.State (StateT(..))
import Control.Monad.Except (Except)
import Blob.Inference.Types (TypeEnv)

data Kind = KType | KArr Kind Kind

type KindEnv = TypeEnv

type KindError = Doc

type KI a = StateT KindEnv (Except KindError) a