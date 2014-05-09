-- This program is free software. It comes without any warranty, to
-- the extent permitted by applicable law. You can redistribute it
-- and/or modify it under the terms of the Do What The Fuck You Want
-- To Public License, Version 2, as published by Sam Hocevar. See
-- http://sam.zoy.org/wtfpl/COPYING for more details.

inod 4 = 2
inod 5 = 3
inod 6 = 4
inod n = inod (n `div` 2 + n `mod` 2) + n `div` 2

main = print $ inod 6953
