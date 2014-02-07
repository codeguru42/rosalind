-- This program is free software. It comes without any warranty, to
-- the extent permitted by applicable law. You can redistribute it
-- and/or modify it under the terms of the Do What The Fuck You Want
-- To Public License, Version 2, as published by Sam Hocevar. See
-- http://sam.zoy.org/wtfpl/COPYING for more details.

import Data.List (sortBy)

import Rosalind

splice dna = concat . exons dna

exons dna introns = exons' dna intronIndexes 0
    where is = map (flip indexes dna) introns
          intronIndexes = sortBy (\x y -> fst x `compare` fst y) . concat $ zipWith (\x y -> zip x . repeat $ length y) is introns
          exons' ""  [] _ = []
          exons' dna [] _ = [dna]
          exons' dna ((x, y):es) n = take (x - n) dna : exons' (drop (x - n + y) dna) es (x + y)

main = do
    contents <- readFile "splc.txt"
    let fasta = parse contents
    let dnas = snd $ unzip fasta
    let dna = head dnas
    let introns = tail dnas
    let splc = splice dna introns
    putStrLn . protein $ transcribe splc
