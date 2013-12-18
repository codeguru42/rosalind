-- This program is free software. It comes without any warranty, to
-- the extent permitted by applicable law. You can redistribute it
-- and/or modify it under the terms of the Do What The Fuck You Want
-- To Public License, Version 2, as published by Sam Hocevar. See
-- http://sam.zoy.org/wtfpl/COPYING for more details.

rabbits :: Int -> Int -> Int
rabbits 1 _ = 1
rabbits 2 _ = 1
rabbits n k = rabbits (n - 1) k + k * rabbits (n - 2) k

main = print $ rabbits n k
    where n = 33
          k = 2
