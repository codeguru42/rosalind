-- This program is free software. It comes without any warranty, to
-- the extent permitted by applicable law. You can redistribute it
-- and/or modify it under the terms of the Do What The Fuck You Want
-- To Public License, Version 2, as published by Sam Hocevar. See
-- http://sam.zoy.org/wtfpl/COPYING for more details.

import Control.Applicative ((<$>))
import Data.List (permutations, intercalate)

format :: [Int] -> String
format xs = intercalate " " (show <$> xs)

main = do
    let ps = permutations [1..n]
    print $ length ps
    mapM_ putStrLn $ map format ps
    where n = 6
