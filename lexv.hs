-- This program is free software. It comes without any warranty, to
-- the extent permitted by applicable law. You can redistribute it
-- and/or modify it under the terms of the Do What The Fuck You Want
-- To Public License, Version 2, as published by Sam Hocevar. See
-- http://sam.zoy.org/wtfpl/COPYING for more details.

import Control.Applicative ((<$>), (<*>))

kMers :: [Char] -> Int -> [String]
kMers _ 0 = [""]
kMers as k = "" : ((:) <$> as <*> kMers as (k-1))

main = do
    let alphabet = "RQXDEWPHYOK"
    let n = 4
    mapM_ putStrLn . tail $ kMers alphabet n
