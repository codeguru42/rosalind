-- This program is free software. It comes without any warranty, to
-- the extent permitted by applicable law. You can redistribute it
-- and/or modify it under the terms of the Do What The Fuck You Want
-- To Public License, Version 2, as published by Sam Hocevar. See
-- http://sam.zoy.org/wtfpl/COPYING for more details.

import Rosalind

main = do
    contents <- readFile "splc.txt"
    let fasta = parse contents
    let dnas = snd $ unzip fasta
    let dna = head dnas
    let exons = tail dnas
    let is = map (flip indexes dna) exons
    mapM_ print is
