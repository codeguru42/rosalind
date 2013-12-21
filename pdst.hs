-- This program is free software. It comes without any warranty, to
-- the extent permitted by applicable law. You can redistribute it
-- and/or modify it under the terms of the Do What The Fuck You Want
-- To Public License, Version 2, as published by Sam Hocevar. See
-- http://sam.zoy.org/wtfpl/COPYING for more details.

import Rosalind

pdst :: String -> String -> Double
pdst s1 s2 = fromIntegral (sum (zipWith (\x y -> if x == y then 0 else 1) s1 s2)) / fromIntegral (length s1)

main = do
    contents <- readFile "pdst.txt"
    let fasta = parse contents
    let dnas = snd $ unzip fasta
    let d = [map (pdst $ dna) dnas | dna <- dnas]
    mapM_ putStrLn $ map format d
