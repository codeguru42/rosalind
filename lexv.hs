-- This program is free software. It comes without any warranty, to
-- the extent permitted by applicable law. You can redistribute it
-- and/or modify it under the terms of the Do What The Fuck You Want
-- To Public License, Version 2, as published by Sam Hocevar. See
-- http://sam.zoy.org/wtfpl/COPYING for more details.

import Control.Applicative ((<$>), (<*>))

kMers :: Int -> [Char] -> [String]
kMers 0 _  = [""]
kMers k as = (:) <$> as <*> kMers (k-1) as

main = do
    let alphabet = "DNA"
    let n = 3
    mapM_ print $ map (flip kMers alphabet) [1..n]
