-- This program is free software. It comes without any warranty, to
-- the extent permitted by applicable law. You can redistribute it
-- and/or modify it under the terms of the Do What The Fuck You Want
-- To Public License, Version 2, as published by Sam Hocevar. See
-- http://sam.zoy.org/wtfpl/COPYING for more details.

import Test.HUnit
import Rosalind

testMakeSuffixTree = "Test makeSuffixTree" ~: expected ~=? actual
    where expected = Main.Node [
              ("bxac", Main.Leaf 3),
              ("c", Main.Leaf 6),
              ("a", Main.Node [("c", Main.Leaf 5), ("bxac", Main.Leaf 2)]),
              ("xa", Main.Node[("c", Main.Leaf 4), ("bxac", Main.Leaf 1)])
            ]
          actual = makeSuffixTree "xabxac"

data SuffixTree = Leaf Int
    | Node [(String, SuffixTree)]
    deriving (Show, Eq)

makeSuffixTree :: String -> SuffixTree
makeSuffixTree = undefined

main = do
    runTestTT testMakeSuffixTree
    contents <- readFile "lcsm.txt"
    let fasta = parse contents
    let dnas = snd $ unzip fasta
    let suffixTree = makeSuffixTree (dnas !! 0)
    putStrLn $ show suffixTree
