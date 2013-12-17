-- This program is free software. It comes without any warranty, to
-- the extent permitted by applicable law. You can redistribute it
-- and/or modify it under the terms of the Do What The Fuck You Want
-- To Public License, Version 2, as published by Sam Hocevar. See
-- http://sam.zoy.org/wtfpl/COPYING for more details.

import Control.Applicative ((<$>))
import Data.List (sort, group, intercalate)

count :: (Ord a) => [a] -> [(a, Int)]
count = (map $ \xs -> (head xs, length xs)) . group . sort

main = do
    contents <- readFile "dna.txt"
    let dna = lines contents
    let counts = snd <$> unzip <$> count <$> dna
    let strings = map show <$> counts
    let formatted = zipWith intercalate (repeat " ") strings
    putStr $ unlines formatted
