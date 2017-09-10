-- This program is free software. It comes without any warranty, to
-- the extent permitted by applicable law. You can redistribute it
-- and/or modify it under the terms of the Do What The Fuck You Want
-- To Public License, Version 2, as published by Sam Hocevar. See
-- http://sam.zoy.org/wtfpl/COPYING for more details.

import qualified Data.MultiMap as MultiMap

data Trie a = Leaf
            | Node [(a, Trie a)]
            deriving Show

makeTrie :: [String] -> Trie Char
makeTrie [] = Leaf
makeTrie xs = Node $ map (\(k, xs) -> (k, makeTrie xs)) root
  where root = MultiMap.assocs $ partitionByFirstChar xs

partitionByFirstChar :: [String] -> MultiMap.MultiMap Char String
partitionByFirstChar [] = MultiMap.empty
partitionByFirstChar ([]:xss) = partitionByFirstChar xss
partitionByFirstChar ((x:xs):xss)
  = MultiMap.insert x xs partitioned
    where partitioned = partitionByFirstChar xss

number :: Int -> Trie a -> (Int, [(Int, Int, a)])
number n Leaf = (n + 1, [])
number n (Node ps) = foldl go (n + 1, []) ps
    where go (m, acc) (c, t) = (m', acc')
            where acc' = (n, m, c) : edges ++ acc
                  (m', edges) = number m t

main = do
  let fileName = "trie.txt"
  input <- readFile fileName
  let strings = lines input
  print strings
  let trie = makeTrie strings
  print trie
  print $ number 1 trie
