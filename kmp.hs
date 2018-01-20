-- This program is free software. It comes without any warranty, to
-- the extent permitted by applicable law. You can redistribute it
-- and/or modify it under the terms of the Do What The Fuck You Want
-- To Public License, Version 2, as published by Sam Hocevar. See
-- http://sam.zoy.org/wtfpl/COPYING for more details.

import Test.HUnit
import Rosalind

tests = test [testLongestPrefix]
longestPrefixTestCases = [(2, "CAG", "CAT")]
testLongestPrefix = map (\(expected, a, b)
                            -> expected ~=? longestPrefix a b)
                        longestPrefixTestCases

longestPrefix (x:xs) (y:ys)
    | x == y = longestPrefix xs ys + 1
    | otherwise = 0

main = do
    runTestTT tests
    contents <- readFile "kmp.txt"
    let fasta = parse contents
    let dnas = snd $ unzip fasta
    mapM_ putStrLn dnas
