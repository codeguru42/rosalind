-- This program is free software. It comes without any warranty, to
-- the extent permitted by applicable law. You can redistribute it
-- and/or modify it under the terms of the Do What The Fuck You Want
-- To Public License, Version 2, as published by Sam Hocevar. See
-- http://sam.zoy.org/wtfpl/COPYING for more details.

import Rosalind (parse, isRevp, indexes)
import Control.Applicative ((<$>))
import Data.List (nub, sort)

isRevp :: String -> Bool
isRevp s = s == revc s

substrings :: String -> Int -> [String]
substrings s n
    | length s >= n = take n s : substrings (tail s) n
    | otherwise = []

formatPair :: (Show a, Show b) => (a, b) -> String
formatPair (x, y) = show x ++ (' ' : show y)

main = do
    contents <- readFile "revp.txt"
    let fasta = parse contents
    let dna = snd $ fasta !! 0
    let subs = concat $ map (substrings dna) [min..max]
    let revps = nub $ filter isRevp subs
    let iss = map (+1) <$> map (flip indexes dna) revps
    mapM_ putStrLn . map formatPair . concat $ zipWith (\is l -> zip is $ repeat l) iss (map length revps)
    where min = 4
          max = 12
