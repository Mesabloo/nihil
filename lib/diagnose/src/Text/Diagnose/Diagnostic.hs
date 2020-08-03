module Text.Diagnose.Diagnostic
( Diagnostic
, diagnostic, (<~<), (<++>)
, printDiagnostic
) where

import Text.Diagnose.Report
import Data.Map (Map)
import qualified Data.Map as Map
import Text.PrettyPrint.ANSI.Leijen
import System.IO (Handle)


data Diagnostic s m a
  = Diagnostic (Files s a) [Report m]


diagnostic :: Diagnostic s m a
diagnostic = Diagnostic mempty mempty

(<~<) :: Diagnostic s m a -> (FilePath, [s a]) -> Diagnostic s m a
Diagnostic files reports <~< (path, content) = Diagnostic (Map.insert path content files) reports

(<++>) :: Diagnostic s m a -> Report m -> Diagnostic s m a
Diagnostic files reports <++> report = Diagnostic files (reports ++ [report])

infixl 5 <++>
infixr 4 <~<


instance (Foldable s, Pretty (s a), Pretty m) => Pretty (Diagnostic s m a) where
  pretty (Diagnostic files reports) = indent 1 (sep (fmap (prettyReport files) reports)) <> line


printDiagnostic :: (Foldable s, Pretty (s a), Pretty m) => Handle -> Diagnostic s m a -> IO ()
printDiagnostic handle diag = displayIO handle (renderPretty 0.9 80 $ pretty diag)
