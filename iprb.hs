-- This program is free software. It comes without any warranty, to
-- the extent permitted by applicable law. You can redistribute it
-- and/or modify it under the terms of the Do What The Fuck You Want
-- To Public License, Version 2, as published by Sam Hocevar. See
-- http://sam.zoy.org/wtfpl/COPYING for more details.

prob :: Int -> Int -> Int -> Double
prob k m n = 1.0 - n1 / (4 * d) - n2 / d - n3 / d
    where t  = k + m + n
          d  = fromIntegral $ t * (t - 1)
          n1 = fromIntegral $ m * (m - 1)
          n2 = fromIntegral $ m * n
          n3 = fromIntegral $ n * (n - 1)

main = print $ prob k m n
    where k = 20
          m = 15
          n = 28