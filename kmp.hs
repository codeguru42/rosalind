-- This program is free software. It comes without any warranty, to
-- the extent permitted by applicable law. You can redistribute it
-- and/or modify it under the terms of the Do What The Fuck You Want
-- To Public License, Version 2, as published by Sam Hocevar. See
-- http://sam.zoy.org/wtfpl/COPYING for more details.

import Test.HUnit
import Rosalind

tests = test [testLongestPrefix, testFailure]
longestPrefixTestCases = [ (2, "CAG", "CAT")
                         , (0, "GAC", "TAC")
                         , (5, "CAGCATGGT", "CAGCAGAG")
                         ]
testLongestPrefix = map (\(expected, a, b)
                            -> expected ~=? longestPrefix a b)
                        longestPrefixTestCases
failureTestCases = [ ( [0, 0, 0, 1, 2, 0, 0, 0, 0, 0, 0, 1, 2, 1, 2, 3, 4, 5, 3, 0, 0]
                     , "CAGCATGGTATCACAGCAGAG"
                     )
                   ]
testFailure = map (\(expected, a) -> expected ~=? failure a)
                  failureTestCases

longestPrefix (x:xs) (y:ys)
    | x == y = longestPrefix xs ys + 1
    | otherwise = 0

failure = undefined

main = do
    runTestTT tests
    contents <- readFile "kmp.txt"
    let fasta = parse contents
    let dnas = snd $ unzip fasta
    mapM_ putStrLn dnas
