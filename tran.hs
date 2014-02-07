-- This program is free software. It comes without any warranty, to
-- the extent permitted by applicable law. You can redistribute it
-- and/or modify it under the terms of the Do What The Fuck You Want
-- To Public License, Version 2, as published by Sam Hocevar. See
-- http://sam.zoy.org/wtfpl/COPYING for more details.

import Rosalind

transitionScore :: Char -> Char -> Int
transitionScore n1 n2
    | n1 == 'A' && n2 == 'G' = 1
    | n1 == 'G' && n2 == 'A' = 1
    | n1 == 'C' && n2 == 'T' = 1
    | n1 == 'T' && n2 == 'C' = 1
    | otherwise = 0

transitionCount :: String -> String -> Int
transitionCount dna1 = sum . zipWith transitionScore dna1

transversionScore :: Char -> Char -> Int
transversionScore n1 n2
    | (n1 == 'A' || n1 == 'G') && (n2 == 'C' || n2 == 'T') = 1
    | (n1 == 'C' || n1 == 'T') && (n2 == 'A' || n2 == 'G') = 1
    | otherwise = 0

transversionCount :: String -> String -> Int
transversionCount dna1 = sum . zipWith transversionScore dna1

main = do
    contents <- readFile "tran.txt"
    let fasta = parse contents
    let dnas = snd $ unzip fasta
    print $ (fromIntegral $ transitionCount (dnas !! 0) (dnas !! 1))
          / (fromIntegral $ transversionCount (dnas !! 0) (dnas !! 1))
