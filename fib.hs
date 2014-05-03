-- This program is free software. It comes without any warranty, to
-- the extent permitted by applicable law. You can redistribute it
-- and/or modify it under the terms of the Do What The Fuck You Want
-- To Public License, Version 2, as published by Sam Hocevar. See
-- http://sam.zoy.org/wtfpl/COPYING for more details.

rabbits :: Int -> Int -> Int
rabbits k n = rabbits' !! (n - 1)
    where rabbits' = [r i | i <- [0..]]
          r 0 = 1
          r 1 = 1
          r n = rabbits' !! (n - 1) + k * rabbits' !! (n - 2)

main = print $ rabbits k n
    where n = 33
          k = 2
