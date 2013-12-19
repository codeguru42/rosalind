-- This program is free software. It comes without any warranty, to
-- the extent permitted by applicable law. You can redistribute it
-- and/or modify it under the terms of the Do What The Fuck You Want
-- To Public License, Version 2, as published by Sam Hocevar. See
-- http://sam.zoy.org/wtfpl/COPYING for more details.

import Rosalind
import Control.Applicative ((<$>))
import Data.List (maximumBy)

transpose :: [[a]] -> [[a]]
transpose xs 
    | null $ head xs = []
    | otherwise = map head xs : transpose (map tail xs)

defaultLookup :: Eq a => b -> a -> [(a, b)] -> b
defaultLookup d key = maybe d id . lookup key 

main = do
    contents <- readFile "cons.txt"
    let fasta = parse contents
    let dnas = snd $ unzip fasta
    let counts = map count $ transpose dnas
    let maxs = map (\x -> maximumBy (\y z -> snd y `compare` snd z) x) counts
    let consensus = map fst maxs
    putStrLn consensus
    let keys = "ACGT"
    let profile = map format [[defaultLookup 0 k c | c <- counts] | k <- keys]
    let profile' = zipWith (\x y -> x : ": " ++ y) keys profile
    mapM_ putStrLn profile'
