module Nihil.Runtime
( evaluate
  -- * Re-exports
, module Nihil.Runtime.Pretty
, module Nihil.Runtime.Core ) where

import Nihil.Runtime.Core
import Nihil.Runtime.Pretty
import qualified Nihil.Runtime.Interpreter as I
import Nihil.Syntax (Expr)
import Text.PrettyPrint.ANSI.Leijen (Doc)
import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (runReaderT)

evaluate :: Expr -> EvalState -> IO (Either Doc Value)
evaluate ex env = runExceptT (runReaderT (I.evaluate ex) env)