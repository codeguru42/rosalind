-- This program is free software. It comes without any warranty, to
-- the extent permitted by applicable law. You can redistribute it
-- and/or modify it under the terms of the Do What The Fuck You Want
-- To Public License, Version 2, as published by Sam Hocevar. See
-- http://sam.zoy.org/wtfpl/COPYING for more details.

hamming :: String -> String -> Int
hamming s1 s2 = sum $ zipWith (\x y -> if x == y then 0 else 1) s1 s2

main = do
    dnaStr <- readFile "hamm.txt"
    let dna = lines dnaStr
    mapM putStrLn dna
    print $ hamming (dna !! 0) (dna !! 1)
