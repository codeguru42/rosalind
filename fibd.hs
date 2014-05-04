-- This program is free software. It comes without any warranty, to
-- the extent permitted by applicable law. You can redistribute it
-- and/or modify it under the terms of the Do What The Fuck You Want
-- To Public License, Version 2, as published by Sam Hocevar. See
-- http://sam.zoy.org/wtfpl/COPYING for more details.

import Test.HUnit.Base ((~=?), (~:))
import Test.HUnit.Text (runTestTT)

fibd :: Int -> Int -> Integer
fibd n m = sum $ fibd' !! (n-1)
    where fibd' = [[fibd'' i j | i <- [0..m-1]] | j <- [0..n]]
          -- `fibd'' i j` is the number of rabbits which are `i` months old after `j` months since the first generation 
          fibd'' 0 0 = 1
          fibd'' _ 0 = 0
          fibd'' 0 j = sum . tail $ fibd' !! (j-1)
          fibd'' i j = fibd' !! (j-1) !! (i-1)

testFibd = "Test fibd" ~: expected ~=? actual
    where expected = [1, 1, 2, 2, 3, 4, 5, 7, 9, 12]
          actual = map (\n -> fibd n 3) [1..10]

main = do
    print $ fibd n m
    where n = 89
          m = 19
