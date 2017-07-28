-- This program is free software. It comes without any warranty, to
-- the extent permitted by applicable law. You can redistribute it
-- and/or modify it under the terms of the Do What The Fuck You Want
-- To Public License, Version 2, as published by Sam Hocevar. See
-- http://sam.zoy.org/wtfpl/COPYING for more details.

data Trie a = Leaf
            | Node a [Trie a]
            deriving Show

makeTrie :: [String] -> Trie Char
makeTrie = undefined

main = do
  let fileName = "trie.txt"
  input <- readFile fileName
  let strings = lines input
  print strings
