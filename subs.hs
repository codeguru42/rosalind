-- This program is free software. It comes without any warranty, to
-- the extent permitted by applicable law. You can redistribute it
-- and/or modify it under the terms of the Do What The Fuck You Want
-- To Public License, Version 2, as published by Sam Hocevar. See
-- http://sam.zoy.org/wtfpl/COPYING for more details.

import Control.Applicative ((<$>))
import Data.List (isPrefixOf, nub, intercalate)

index :: Eq a => [a] -> [a] -> Int
index xs ys = index' xs ys 0
    where index' _  [] _ = -1
          index' xs ys n = if xs `isPrefixOf` ys then n else index' xs (tail ys) (n + 1)
          

indexes :: Eq a => [a] -> [a] -> [Int]
indexes xs [] = []
indexes xs ys = nub $ if i == -1 then is else i:is
    where i = index xs ys
          is = map (+1) (indexes xs (tail ys))

format :: [Int] -> String
format xs = intercalate " " (show <$> xs)

main = do
    contents <- readFile "subs.txt"
    let dna = lines contents
    print dna
    putStrLn . format . map (+1) $ indexes (dna !! 1) (dna !! 0)
