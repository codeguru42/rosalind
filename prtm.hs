-- This program is free software. It comes without any warranty, to
-- the extent permitted by applicable law. You can redistribute it
-- and/or modify it under the terms of the Do What The Fuck You Want
-- To Public License, Version 2, as published by Sam Hocevar. See
-- http://sam.zoy.org/wtfpl/COPYING for more details.

mass :: Char -> Double
mass p = m
    where Just m = lookup p masses
          masses = [ ('A', 71.03711 ), ('C', 103.00919), ('D', 115.02694), ('E', 129.04259)
                   , ('F', 147.06841), ('G', 57.02146 ), ('H', 137.05891), ('I', 113.08406)
                   , ('K', 128.09496), ('L', 113.08406), ('M', 131.04049), ('N', 114.04293)
                   , ('P', 97.05276 ), ('Q', 128.05858), ('R', 156.10111), ('S', 87.03203 )
                   , ('T', 101.04768), ('V', 99.06841 ), ('W', 186.07931), ('Y', 163.06333)
                   ]

main = do
    contents <- readFile "prtm.txt"
    let protein = lines contents !! 0
    print protein
    print . sum $ map mass protein
