{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Nihil.Runtime
( eval
  -- * Re-exports
, module Nihil.Runtime.Core ) where

import Nihil.Runtime.Core
import Nihil.Runtime.Interpreter