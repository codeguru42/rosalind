-- This program is free software. It comes without any warranty, to
-- the extent permitted by applicable law. You can redistribute it
-- and/or modify it under the terms of the Do What The Fuck You Want
-- To Public License, Version 2, as published by Sam Hocevar. See
-- http://sam.zoy.org/wtfpl/COPYING for more details.

import Data.List (maximumBy)

gc :: String -> Double
gc dna = fromIntegral (length gcs) / fromIntegral (length dna)
    where gcs = filter (\x -> x == 'G' || x == 'C') dna

parse :: String -> [(String, String)]
parse s = parse' $ lines s
    where parse' [] = []
          parse' [""] = []
          parse' (first:rest) = case first of
                ('>':name) -> (name, dna) : parse' rest'
                _          -> error ("Expected '>' but got '" ++ first ++ "'")
            where (dna, rest') = parseDna rest
                  parseDna [] = ("", [""])
                  parseDna (first':rest') = case first' of
                    ('>':name) -> ("", first':rest')
                    _          -> (first' ++ dna', rest'')
                        where (dna', rest'') = parseDna rest'

main = do
    contents <- readFile "gc.txt"
    let dna = parse contents
    let gcs = map (\(name,strand) -> (name, gc strand)) dna
    let maxGc = maximumBy (\(name1, gc1) (name2, gc2) -> compare gc1 gc2) gcs
    putStrLn $ fst maxGc
    print $ snd maxGc * 100.0
