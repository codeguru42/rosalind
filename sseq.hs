-- This program is free software. It comes without any warranty, to
-- the extent permitted by applicable law. You can redistribute it
-- and/or modify it under the terms of the Do What The Fuck You Want
-- To Public License, Version 2, as published by Sam Hocevar. See
-- http://sam.zoy.org/wtfpl/COPYING for more details.

import Data.List (elemIndex, tails)
import Data.Maybe (fromMaybe)

import Rosalind

spliceSequence :: String -> String -> [Int]
spliceSequence dna1 dna2 = spliceSequence' dna1 dna2 0
    where spliceSequence' _ [] _ = []
          spliceSequence' dna1 (d:ds) n = n + k : spliceSequence' (drop (k + 1) dna1) ds (n + k + 1)
            where Just k = d `elemIndex` dna1   

main = do
    contents <- readFile "sseq.txt"
    let fasta = parse contents
    let dnas = snd $ unzip fasta
    putStrLn . format . map (+1) $ spliceSequence (dnas !! 0) (dnas !! 1)
