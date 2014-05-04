-- This program is free software. It comes without any warranty, to
-- the extent permitted by applicable law. You can redistribute it
-- and/or modify it under the terms of the Do What The Fuck You Want
-- To Public License, Version 2, as published by Sam Hocevar. See
-- http://sam.zoy.org/wtfpl/COPYING for more details.

import Test.HUnit.Base ((~=?), (~:))
import Test.HUnit.Text (runTestTT)

fibd :: Int -> Int -> Int
fibd n m = undefined

testFibd = "Test fibd" ~: expected ~=? actual
    where expected = [1, 1, 2, 2, 3, 4, 5, 7]
          actual = map (\n -> fibd n 3) [1..8]

main = runTestTT testFibd
