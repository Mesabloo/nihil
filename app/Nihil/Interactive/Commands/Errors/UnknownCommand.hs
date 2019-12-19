-- The Great Nihil Compiler
-- Copyright (c) 2019 Mesabloo

-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.

-- You should have received a copy of the GNU General Public License
-- along with this program. If not, see <http://www.gnu.org/licenses/>.

module Nihil.Interactive.Commands.Errors.UnknownCommand where

import Nihil.Interactive.Command
import Text.Megaparsec (manyTill, anySingle, eof, (<|>))
import qualified Text.Megaparsec.Char as C
import Data.List (intercalate)

makeCommandError :: CommandParser String
makeCommandError = do
    cmd <- C.space *> manyTill anySingle (C.space1 <|> eof)
    pure $ "Unknown command `" <> cmd <> "`\n" <> maybeYouWanted (':':cmd) commands


-- | The levenshtein algorithm for suggesting typing error possibilities.
levenshtein :: String -> String -> Int
levenshtein s1 s2
    | length s1 > length s2 = levenshtein s2 s1
    | length s1 < length s2 = let d = length s2 - length s1
                              in  d + levenshtein s1 (take (length s2 - d) s2)
levenshtein "" ""           = 0
levenshtein s1 s2
    | last s1 == last s2    = levenshtein (init s1) (init s2)
    | otherwise             = minimum [ 1 + levenshtein (init s1) s2
                                      , 1 + levenshtein s1 (init s2)
                                      , 1 + levenshtein (init s1) (init s2) ]

-- | A wrapper around the levenshtein algorithm to prettify the output.
maybeYouWanted :: String -> [String] -> String
maybeYouWanted source choices =
    let s = intercalate ", "
            . takeMax 3
            . filter (/= "")
            . map (\item ->
                if levenshtein source item > 3
                then ""
                else intercalate item ["\"", "\""])
            $ choices
    in
        if s /= ""
        then "Perhaps you meant " <> s <> "?"
        else ""
  where takeMax :: Int -> [a] -> [a]
        takeMax n xs | n > length xs = xs
                     | otherwise     = take n xs