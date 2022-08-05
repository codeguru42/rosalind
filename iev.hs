-- This program is free software. It comes without any warranty, to
-- the extent permitted by applicable law. You can redistribute it
-- and/or modify it under the terms of the Do What The Fuck You Want
-- To Public License, Version 2, as published by Sam Hocevar. See
-- http://sam.zoy.org/wtfpl/COPYING for more details.

main = do
    let counts = [16827, 18384, 16861, 17956, 18182, 16775]
    let ps = [1.0, 1.0, 1.0, 0.75, 0.5, 0.0]
    print . (2*) . sum $ zipWith (*) counts ps
