-- This program is free software. It comes without any warranty, to
-- the extent permitted by applicable law. You can redistribute it
-- and/or modify it under the terms of the Do What The Fuck You Want
-- To Public License, Version 2, as published by Sam Hocevar. See
-- http://sam.zoy.org/wtfpl/COPYING for more details.

n `choose` k = product [m+1..n] `div` product [1..n-m]
    where m = max k $ n - k

main = print $ (sum $ map (\k -> (n `choose` k) `mod` 10^6) [m..n]) `mod` 10^6
    where n = 1641
          m = 927
