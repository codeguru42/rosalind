-- This program is free software. It comes without any warranty, to
-- the extent permitted by applicable law. You can redistribute it
-- and/or modify it under the terms of the Do What The Fuck You Want
-- To Public License, Version 2, as published by Sam Hocevar. See
-- http://sam.zoy.org/wtfpl/COPYING for more details.

complement :: Char -> Char
complement 'A' = 'T'
complement 'T' = 'A'
complement 'C' = 'G'
complement 'G' = 'C'
complement x = error "Unknown neucleotide"

main = do
    dnaStr <- readFile "revc.txt"
    let dna = (lines dnaStr) !! 0
    putStrLn $ (reverse . map complement) dna
