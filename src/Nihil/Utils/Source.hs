{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

{-| Locating nodes of the AST inside the source code is very important for error
    debugging. -}

module Nihil.Utils.Source
( -- * Getting positions in source files
  Position(..), SourcePos(..), sourceLine, sourceColumn, sourceFile, fromSourcePos, locate
  -- * Attaching positions to values
, Located, annotated, location ) where

import Control.Lens (makeLenses, (^.))
import qualified Text.Megaparsec as MP (SourcePos(..), unPos, Pos)
import Control.Applicative (Const(..))
import Nihil.Utils.Annotation
import Control.Arrow ((>>>), (&&&), (<<<))
import Data.Function (on)
import Text.PrettyPrint.ANSI.Leijen

-- | A wrapper around the type 'Int'.
newtype Position = Pos { unPos :: Int }
  deriving (Eq, Ord, Num)

instance Semigroup Position where
    (<>) = (+)

instance Monoid Position where
    mempty = Pos 1

instance Show Position where
    show (Pos a) = show a


data SourcePos
    = NoSource                  -- ^ No source file has been found or given
    | Loc
    { _sourceLine   :: Position -- ^ The source file line number
    , _sourceColumn :: Position -- ^ The source file column number
    , _sourceFile   :: FilePath -- ^ The source file name
    }
  deriving Eq
makeLenses ''SourcePos

instance Ord SourcePos where
    NoSource <= NoSource             = True
    (Loc l1 c1 _ ) <= (Loc l2 c2 _ ) = l1 < l2 || (l1 == l2 && c1 <= c2)
    _ <= _                           = False

-- | Only use for debugging
instance Show SourcePos where
    show NoSource = "<no source>:1:1"
    show loc      = "<" <> loc ^. sourceFile <> ">:" <> show (loc ^. sourceLine) <> ":" <> show (loc ^. sourceColumn)
#if defined(DEBUG)
                         <> ":" <> show (loc ^. indentLevel)
#endif


-- | An annotation containing the annotated value (via 'Const').
--
--   It is defined in terms of 'Annotated' for the sake of recursion schemes
--   and to avoid redefining something which could have been defined easily.
type Located a = Annotated (Const a) SourcePos

instance {-# OVERLAPPING #-} Show a => Show (Located a) where
    show = (annotated >>> show) &&& (location >>> show) >>> uncurry (<>)

instance {-# OVERLAPPING #-} Eq a => Ord (Located a) where
    (<=) = (<=) `on` location

instance {-# OVERLAPPING #-} Eq a => Eq (Located a) where
    (==) = (==) `on` annotated

-- | Unwraps the position of a 'Located' value.
--
--   See 'extract''.
location :: Located a -> SourcePos
location = extract'

-- | Unwraps the annotated value from a 'Located' value.
--
--   See 'unwrap''.
annotated :: Located a -> a
annotated = unwrap' >>> getConst


-- | Simple converter function between megaparsec's 'MP.SourcePos' and 'SourcePos'.
--
--   It takes an indentation level as well to fit it in the returned 'SourcePos'.
fromSourcePos :: MP.SourcePos -> SourcePos
fromSourcePos pos =
    let line = asPos (MP.sourceLine pos)
        col  = asPos (MP.sourceColumn pos)
        file = MP.sourceName pos
    in Loc line col file

asPos :: MP.Pos -> Position
asPos = Pos <<< MP.unPos

-- | Annotates a value with a 'SourcePos'.
--
--   It is more convenient to use 'locate' than its actual definition:
--
--   @locate = 'Const' '>>>' 'flip' 'singleton'@
locate :: a -> SourcePos -> Located a
locate = Const >>> flip singleton

instance Pretty a => Pretty (Located a) where
    pretty = annotated >>> pretty
