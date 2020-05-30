{-# LANGUAGE CPP #-}

module Nihil.Syntax.Concrete.Debug where

import qualified Text.Megaparsec as MP

#if defined(DEBUGGING)
import qualified Text.Megaparsec.Debug as MP

{-| An alias to 'MP.dbg' used to debug a megaparsec's parser.

    It logs many useful infos, such as the input stream, if there were any error, and the result if one is present.
-}
debug :: (MP.Stream s, MP.ShowErrorComponent e, Show a) => String -> MP.ParsecT e s m a -> MP.ParsecT e s m a
debug = MP.dbg
#else
{-| This function is eliminated at compile-time. It is provided to conform to some code regardless of the debugging
    being turned on or off. -}
debug :: MP.MonadParsec e s m => String -> m a -> m a
debug = flip const

{-# NOINLINE debug #-}

{-# RULES
"debug/eliminate" forall n x. debug n x = x
#-}
#endif