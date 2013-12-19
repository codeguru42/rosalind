-- This program is free software. It comes without any warranty, to
-- the extent permitted by applicable law. You can redistribute it
-- and/or modify it under the terms of the Do What The Fuck You Want
-- To Public License, Version 2, as published by Sam Hocevar. See
-- http://sam.zoy.org/wtfpl/COPYING for more details.

module Rosalind
  ( count
  , parse
  , revc
  , index
  , indexes
  , format
  , formatPair
  , transcribe
  , protein
  , stopCodons
  , chunksOf
  ) where

import Control.Applicative ((<$>))
import Data.List (intercalate, isPrefixOf, nub, group, sort)

count :: (Ord a) => [a] -> [(a, Int)]
count = (map $ \xs -> (head xs, length xs)) . group . sort

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

complement :: Char -> Char
complement 'A' = 'T'
complement 'T' = 'A'
complement 'C' = 'G'
complement 'G' = 'C'
complement x = error "Unknown neucleotide"

revc :: String -> String
revc = reverse . map complement

index :: Eq a => [a] -> [a] -> Int
index xs ys = index' xs ys 0
    where index' _  [] _ = -1
          index' xs ys n = if xs `isPrefixOf` ys then n else index' xs (tail ys) (n + 1)
          

indexes :: Eq a => [a] -> [a] -> [Int]
indexes xs [] = []
indexes xs ys = nub $ if i == -1 then is else i:is
    where i = index xs ys
          is = map (+1) (indexes xs (tail ys))

format :: [Int] -> String
format xs = intercalate " " (show <$> xs)

formatPair :: (Show a, Show b) => (a, b) -> String
formatPair (x, y) = show x ++ (' ' : show y)

transcribe :: String -> String
transcribe = map (\x -> case x of 
    'T' -> 'U'
    _   -> x)

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

stopCodons :: [String]
stopCodons = ["UAG", "UGA", "UAA"]

protein :: String -> String
protein = concat . map codon . takeWhile (\x -> not $ x `elem` stopCodons) . chunksOf 3
