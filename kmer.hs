-- This program is free software. It comes without any warranty, to
-- the extent permitted by applicable law. You can redistribute it
-- and/or modify it under the terms of the Do What The Fuck You Want
-- To Public License, Version 2, as published by Sam Hocevar. See
-- http://sam.zoy.org/wtfpl/COPYING for more details.

import Control.Applicative ((<$>), (<*>))
import Data.List (isPrefixOf)
import Rosalind

kMers _ 0 = [[]]
kMers alphabet n = (:) <$> alphabet <*> kMers alphabet (n - 1)

countKmer [] _ = 0
countKmer dna kmer = if kmer `isPrefixOf` dna then 1 + rest else rest
    where rest = countKmer (tail dna) kmer

main = do
    contents <- readFile "kmer.txt"
    let fasta = parse contents
    let dnas = snd $ unzip fasta
    let dna = dnas !! 0
    let alphabet = "ACGT"
    let fourMers = kMers alphabet 4
    let composition = map (countKmer dna) fourMers 
    putStrLn $ format composition
