module Nihil.Syntax.Concrete.Lexer
( withPosition, lexeme ) where

import Nihil.Syntax.Common (Lexer, currentIndent)
import Nihil.Utils.Source (Located, withIndent, locate)
import Nihil.Syntax.Concrete.Lexer.Comment (pLineComment, pBlockComment)
import Control.Lens (use)
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char.Lexer as MPL
import qualified Data.Char as Ch (ord)

-- | Runs a lexer computation and annotates its result with a 'Nihil.Utils.Source.SourcePos'.
withPosition :: Lexer a -> Lexer (Located a)
withPosition l = do
    indent <- use currentIndent
    pos    <- withIndent indent <$> MP.getSourcePos
    a      <- l
    pure (locate a pos)

-- | Skips any space and comment right after running a 'Lexer' computation.
lexeme :: Lexer a -> Lexer a
lexeme = MPL.lexeme (MPL.space space1' pLineComment pBlockComment)

space1' :: Lexer ()
space1' = MP.skipSome (MP.satisfy isSpace)

isSpace :: Char -> Bool
isSpace c =
    let code = Ch.ord c
    in code == 9 || code == 32 || code == 160 || code == 8200 || code == 8201 || code == 8202