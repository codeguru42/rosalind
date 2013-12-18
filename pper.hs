-- This program is free software. It comes without any warranty, to
-- the extent permitted by applicable law. You can redistribute it
-- and/or modify it under the terms of the Do What The Fuck You Want
-- To Public License, Version 2, as published by Sam Hocevar. See
-- http://sam.zoy.org/wtfpl/COPYING for more details.

perm :: Int -> Int -> Int
perm n k = foldr (\x y -> (x * y) `mod` 1000000) 1 [n-k+1..n]

main = print $ perm n k
    where n = 87
          k = 8