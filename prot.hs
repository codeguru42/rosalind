 -- This program is free software. It comes without any warranty, to
 -- the extent permitted by applicable law. You can redistribute it
 -- and/or modify it under the terms of the Do What The Fuck You Want
 -- To Public License, Version 2, as published by Sam Hocevar. See
 -- http://sam.zoy.org/wtfpl/COPYING for more details.

codon :: String -> String
codon s = c
    where Just c = lookup s codons
          codons = [ ("UUU", "F"), ("UUC", "F"), ("UUA", "L"), ("UUG", "L")
                   , ("UCU", "S"), ("UCC", "S"), ("UCA", "S"), ("UCG", "S")
                   , ("UAU", "Y"), ("UAC", "Y"), ("UAA", "" ), ("UAG", "" )
                   , ("UGU", "C"), ("UGC", "C"), ("UGA", "" ), ("UGG", "W")
                   , ("CUU", "L"), ("CUC", "L"), ("CUA", "L"), ("CUG", "L")
                   , ("CCU", "P"), ("CCC", "P"), ("CCA", "P"), ("CCG", "P")
                   , ("CAU", "H"), ("CAC", "H"), ("CAA", "Q"), ("CAG", "Q")
                   , ("CGU", "R"), ("CGC", "R"), ("CGA", "R"), ("CGG", "R")
                   , ("AUU", "I"), ("AUC", "I"), ("AUA", "I"), ("AUG", "M")
                   , ("ACU", "T"), ("ACC", "T"), ("ACA", "T"), ("ACG", "T")
                   , ("AAU", "N"), ("AAC", "N"), ("AAA", "K"), ("AAG", "K")
                   , ("AGU", "S"), ("AGC", "S"), ("AGA", "R"), ("AGG", "R")
                   , ("GUU", "V"), ("GUC", "V"), ("GUA", "V"), ("GUG", "V")
                   , ("GCU", "A"), ("GCC", "A"), ("GCA", "A"), ("GCG", "A")
                   , ("GAU", "D"), ("GAC", "D"), ("GAA", "E"), ("GAG", "E")
                   , ("GGU", "G"), ("GGC", "G"), ("GGA", "G"), ("GGG", "G")
                   ]   

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

main = do
    rnaStr <- readFile "prot.txt"
    let rnas = lines rnaStr
    putStrLn . concat . map codon $ chunksOf 3 (rnas !! 0)
