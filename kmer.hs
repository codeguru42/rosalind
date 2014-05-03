-- This program is free software. It comes without any warranty, to
-- the extent permitted by applicable law. You can redistribute it
-- and/or modify it under the terms of the Do What The Fuck You Want
-- To Public License, Version 2, as published by Sam Hocevar. See
-- http://sam.zoy.org/wtfpl/COPYING for more details.

import Control.Applicative ((<$>), (<*>))
import Rosalind

kMers _ 0 = [[]]
kMers alphabet n = (:) <$> alphabet <*> kMers alphabet (n - 1)

main = do
    contents <- readFile "kmer.txt"
    let fasta = parse contents
    let dnas = snd $ unzip fasta
    let dna = dnas !! 0
    let alphabet = "ACGT"
    mapM_ putStrLn $ kMers alphabet 4
