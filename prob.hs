-- This program is free software. It comes without any warranty, to
-- the extent permitted by applicable law. You can redistribute it
-- and/or modify it under the terms of the Do What The Fuck You Want
-- To Public License, Version 2, as published by Sam Hocevar. See
-- http://sam.zoy.org/wtfpl/COPYING for more details.

import Rosalind
import Data.List.Split (splitOn)

logProb :: String -> Double -> Double
logProb dna gc = sum $ map (flip (defaultLookup 0) logs) dna 
    where logGC = logBase 10 gc - logBase 10 2.0
          logAT = logBase 10 (1 - gc) - logBase 10 2.0
          logs = [ ('A', logAT)
                 , ('T', logAT)
                 , ('G', logGC)
                 , ('C', logGC)] 

main = do
    contents <- readFile "prob.txt"
    let ls = lines contents
    let dna = ls !! 0
    let gcs = map read . splitOn " " $ ls !! 1 :: [Double]
    putStrLn . format $ map (logProb dna) gcs
