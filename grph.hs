-- This program is free software. It comes without any warranty, to
-- the extent permitted by applicable law. You can redistribute it
-- and/or modify it under the terms of the Do What The Fuck You Want
-- To Public License, Version 2, as published by Sam Hocevar. See
-- http://sam.zoy.org/wtfpl/COPYING for more details.

import Rosalind
import Control.Applicative (liftA2)
import Data.List (isSuffixOf)

overlap :: String -> String -> Bool
overlap s1 s2 = (take 3 s2) `isSuffixOf` s1

overlapGraph :: [(String, String)] -> [(String, String)]
overlapGraph fasta = map (\((x, _), (y, _)) -> (x, y)) pairs'
    where pairs' = filter (\(x, y) -> snd x `overlap` snd y) pairs
          pairs = filter (\(x, y) -> x /= y) $ liftA2 (,) fasta fasta

main = do
    contents <- readFile "grph.txt"
    let fasta = parse contents
    mapM_ putStrLn . map (\(x, y) -> x ++ " " ++ y) $ overlapGraph fasta
