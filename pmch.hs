-- This program is free software. It comes without any warranty, to
-- the extent permitted by applicable law. You can redistribute it
-- and/or modify it under the terms of the Do What The Fuck You Want
-- To Public License, Version 2, as published by Sam Hocevar. See
-- http://sam.zoy.org/wtfpl/COPYING for more details.

import Rosalind

factorial :: Integer -> Integer
factorial n = product [1..n]

main = do
    contents <- readFile "pmch.txt"
    let fasta = parse contents
    let rnas = snd $ unzip fasta
    let counts = count $ rnas !! 0
    print $ factorial (fromIntegral $ defaultLookup 1 'A' counts)
          * factorial (fromIntegral $ defaultLookup 1 'G' counts)
