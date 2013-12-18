-- This program is free software. It comes without any warranty, to
-- the extent permitted by applicable law. You can redistribute it
-- and/or modify it under the terms of the Do What The Fuck You Want
-- To Public License, Version 2, as published by Sam Hocevar. See
-- http://sam.zoy.org/wtfpl/COPYING for more details.

module Rosalind
  ( parse
  , revc
  , isRevp
  , index
  , indexes
  , format
  ) where

import Control.Applicative ((<$>))
import Data.List (intercalate, isPrefixOf, nub)

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
