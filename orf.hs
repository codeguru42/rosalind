-- This program is free software. It comes without any warranty, to
-- the extent permitted by applicable law. You can redistribute it
-- and/or modify it under the terms of the Do What The Fuck You Want
-- To Public License, Version 2, as published by Sam Hocevar. See
-- http://sam.zoy.org/wtfpl/COPYING for more details.

import Rosalind
import Data.List (nub, sort)

startCodon :: String
startCodon = "AUG"

proteins :: String -> [String]
proteins dna = nub $ proteins' dna ++ proteins' (revc dna)
    where proteins' dna = map protein orfs
            where orfs' = map (\x -> drop x rna) starts
                  orfs = filter (\xs -> anyElem stopCodons (chunksOf 3 xs)) orfs'
                  starts = indexes startCodon rna
                  rna = transcribe dna

anyElem :: Eq a => [a] -> [a] -> Bool
anyElem xs ys = any (`elem` ys) xs

main = do
    contents <- readFile "orf.txt"
    let fasta = parse contents
    let dna = snd $ fasta !! 0
    mapM_ putStrLn $ proteins dna