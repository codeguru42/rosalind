-- This program is free software. It comes without any warranty, to
-- the extent permitted by applicable law. You can redistribute it
-- and/or modify it under the terms of the Do What The Fuck You Want
-- To Public License, Version 2, as published by Sam Hocevar. See
-- http://sam.zoy.org/wtfpl/COPYING for more details.

import Control.Applicative ((*>), (<$>))
import Data.Either (rights)
import Data.List (union, intersect, (\\))
import Text.ParserCombinators.Parsec
import Text.Parsec.Token

set = do
    char '{'
    result <- list
    char '}'
    return result

list = sepBy1 cell (char ',')
cell = many space *> many (noneOf ",}")

parseSet :: String -> Either ParseError [String]
parseSet = parse set "(unknown)"

showSet s = '{' : showList s ++ "}"
    where showList [x] = show x
          showList (x:xs) = show x ++ ", " ++ showList xs 

main = do
    contents <- readFile "seto.txt"
    let ls = lines contents
    let n = read $ ls !! 0 :: Int
    let sets = map (map read) $ rights . map parseSet $ drop 1 ls :: [[Int]]
    let a = sets !! 0
    let b = sets !! 1
    putStrLn $ showSet (a `union` b)
    putStrLn $ showSet (a `intersect` b)
    putStrLn $ showSet (a \\ b)
    putStrLn $ showSet (b \\ a)
    putStrLn $ showSet ([1..n] \\ a)
    putStrLn $ showSet ([1..n] \\ b)
