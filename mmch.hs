-- This program is free software. It comes without any warranty, to
-- the extent permitted by applicable law. You can redistribute it
-- and/or modify it under the terms of the Do What The Fuck You Want
-- To Public License, Version 2, as published by Sam Hocevar. See
-- http://sam.zoy.org/wtfpl/COPYING for more details.

import Rosalind

factorial :: Integer -> Integer
factorial n = product [1..n]

main = do
    contents <- readFile "mmch.txt"
    let fasta = parse contents
    let rnas = snd $ unzip fasta
    let counts = count $ rnas !! 0
    let lookup x = fromIntegral $ defaultLookup 1 x counts
    let aCount = lookup 'A'
    let uCount = lookup 'U'
    let gCount = lookup 'G'
    let cCount = lookup 'C'
    let auMax = max aCount uCount
    let auMin = min aCount uCount
    let gcMax = max gCount cCount
    let gcMin = min gCount cCount
    print counts
    print $ (auMax `choose` auMin) * (factorial auMin)
        * (gcMax `choose` gcMin) * (factorial gcMin)
