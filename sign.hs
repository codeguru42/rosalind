-- This program is free software. It comes without any warranty, to
-- the extent permitted by applicable law. You can redistribute it
-- and/or modify it under the terms of the Do What The Fuck You Want
-- To Public License, Version 2, as published by Sam Hocevar. See
-- http://sam.zoy.org/wtfpl/COPYING for more details.

import Rosalind
import Data.List (permutations)

countSignedPermutations :: Integer -> Integer
countSignedPermutations n = 2 ^ n * product [1..n]

main = do
    let perms = permutations [1..n]
    print $ countSignedPermutations n
    mapM_ putStrLn $ map format perms
    where n = 2
