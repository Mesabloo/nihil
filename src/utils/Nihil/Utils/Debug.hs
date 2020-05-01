{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}

{-| Debugging tools for the compiler. Enabling them may cause
    the compiler to slow down and provide much unwanted output.
    Use with caution.
-}

module Nihil.Utils.Debug
( -- * Unconditional debugging
  info, error, warn, log
  -- * Conditional debugging
, infoWhen, errorWhen, warnWhen, logWhen ) where

import Prelude (Show, Bool, flip, const)

#if defined(DEBUGGING)
import Prelude (String, show, Bool(..), pure, putStrLn)
import Text.PrettyPrint.ANSI.Leijen (red, green, yellow, blue, brackets, text, putDoc, Doc, dullred, dullgreen, dullyellow, dullblue, (<+>))
import System.IO.Unsafe (unsafePerformIO)

debug :: Doc -> a -> a
debug !doc x = unsafePerformIO do
    putDoc doc
    putStrLn ""
    pure x

-- | This function is specialized when given a 'String', so that it doesn't
--   include the @"@s in the debugging.
info, error, warn, log :: Show a => a -> b -> b
-- | Outputs some information on the screen, recognizable by a little @[ INFO ]@
--   showing up in front of the information.
info x act  = info' (show x) act
-- | Outputs some error on the screen, recognizable by a little @[ERROR ]@
--   showing up in front of the error.
--
--   This function doesn't stop your program. If you want to stop your program,
--   consider using @'error'@ or even @'Nihil.Utils.Impossible.impossible'@.
error x act = error' (show x) act
-- | Drops a warning on the screen, recognizable by a little @[ WARN ]@ prefix.
warn x act  = warn' (show x) act
-- | Outputs some logging information on the screen, recognizable by a little @[ LOG  ]@
--   showing up in front of the information.
log x act   = log' (show x) act

infoWhen, errorWhen, warnWhen, logWhen :: Show a => Bool -> a -> b -> b
-- | Behaves like 'info' only if the condition is met.
--
--   Else it just does nothing
infoWhen True   = info
infoWhen False  = flip const
-- | Behaves like 'error' only if the condition is met.
--
--   Else it just does nothing
errorWhen True  = error
errorWhen False = flip const
-- | Behaves like 'warn' only if the condition is met.
--
--   Else it just does nothing
warnWhen True   = warn
warnWhen False  = flip const
-- | Behaves like 'log' only if the condition is met.
--
--   Else it just does nothing
logWhen True    = log
logWhen False   = flip const

info', error', warn', log' :: String -> b -> b
info' x act  = debug doc act
  where doc = brackets (green (text " INFO ")) <+> dullgreen (text x)
error' x act = debug doc act
  where doc = brackets (red (text "ERROR ")) <+> dullred (text x)
warn' x act  = debug doc act
  where doc = brackets (yellow (text " WARN ")) <+> dullyellow (text x)
log' x act   = debug doc act
  where doc = brackets (blue (text " LOG  ")) <+> dullblue (text x)

{-# INLINE[1] info  #-}
{-# INLINE[1] error #-}
{-# INLINE[1] warn #-}
{-# INLINE[1] log #-}

{-# RULES
"info/String"  info  = info'
"error/String" error = error'
"warn/String"  warn  = warn'
"log/String"   log   = log'
#-}
#else
-- | This debugging function is eliminated at compile time if the debugging
--   flag has not been activated.
--
--   * If you are using @cabal@, try adding @-f debug@ to your command-line.
--
--   * If you are using @stack@, try adding @--flag nihil:debug@ to your command-line.
info, error, warn, log :: Show a => a -> b -> b
-- | Outputs some information on the screen, recognizable by a little @[ INFO ]@
--   showing up in front of the information.
info        = flip const
-- | Outputs some error on the screen, recognizable by a little @[ERROR ]@
--   showing up in front of the error.
--
--   This function doesn't stop your program. If you want to stop your program,
--   consider using @'error'@ or even @'Nihil.Utils.Impossible.impossible'@.
error       = flip const
-- | Drops a warning on the screen, recognizable by a little @[ WARN ]@ prefix.
warn        = flip const
-- | Outputs some logging information on the screen, recognizable by a little @[ LOG  ]@
--   showing up in front of the information.
log         = flip const

-- | This function is also eliminated at compile time when debugging options
--   are not activated.
infoWhen, errorWhen, warnWhen, logWhen :: Show a => Bool -> a -> b -> b
-- | Behaves like 'info' only if the condition is met.
--
--   Else it just does nothing
infoWhen  _ = flip const
-- | Behaves like 'error' only if the condition is met.
--
--   Else it just does nothing
errorWhen _ = flip const
-- | Behaves like 'warn' only if the condition is met.
--
--   Else it just does nothing
warnWhen  _ = flip const
-- | Behaves like 'log' only if the condition is met.
--
--   Else it just does nothing
logWhen   _ = flip const

{-# NOINLINE info  #-}
{-# NOINLINE error #-}
{-# NOINLINE warn  #-}
{-# NOINLINE log   #-}
{-# NOINLINE infoWhen  #-}
{-# NOINLINE errorWhen #-}
{-# NOINLINE warnWhen  #-}
{-# NOINLINE logWhen   #-}

{-# RULES
"info/eliminate"  forall x y. info x y  = y
"error/eliminate" forall x y. error x y = y
"warn/eliminate"  forall x y. warn x y  = y
"log/eliminate"   forall x y. log x y   = y

"infoWhen/eliminate"  forall b x y. infoWhen b x y  = y
"errorWhen/eliminate" forall b x y. errorWhen b x y = y
"warnWhen/eliminate"  forall b x y. warnWhen b x y  = y
"logWhen/eliminate"   forall b x y. logWhen b x y   = y
#-}
#endif