-- This program is free software. It comes without any warranty, to
-- the extent permitted by applicable law. You can redistribute it
-- and/or modify it under the terms of the Do What The Fuck You Want
-- To Public License, Version 2, as published by Sam Hocevar. See
-- http://sam.zoy.org/wtfpl/COPYING for more details.

import Rosalind

number :: Int -> Rosalind.Trie a -> (Int, [(Int, Int, a)])
number n Rosalind.Leaf = (n + 1, [])
number n (Rosalind.Node ps) = foldl go (n + 1, []) ps
  where go (m, acc) (c, t) = (m', acc')
          where acc' = (n, m, c) : edges ++ acc
                (m', edges) = number m t

printNumbers :: [(Int, Int, Char)] -> IO()
printNumbers xs = mapM_ printNumbers' xs
  where printNumbers' (n, m, c) = putStrLn $ concat [show n, " ", show m, " ", [c]]

main = do
  let fileName = "trie.txt"
  input <- readFile fileName
  let strings = lines input
  let trie = Rosalind.makeTrie strings
  let numbers = snd $ number 1 trie
  printNumbers numbers
