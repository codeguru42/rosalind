-- This program is free software. It comes without any warranty, to
-- the extent permitted by applicable law. You can redistribute it
-- and/or modify it under the terms of the Do What The Fuck You Want
-- To Public License, Version 2, as published by Sam Hocevar. See
-- http://sam.zoy.org/wtfpl/COPYING for more details.

import Data.Maybe (fromMaybe)
import Network.HTTP
import Text.Regex.Posix

import Rosalind

fastaUrl :: String -> String
fastaUrl uniprotId = baseUrl ++ uniprotId ++ ".fasta"
    where baseUrl = "http://www.uniprot.org/uniprot/"

getUniprotFasta :: String -> IO (String, String)
getUniprotFasta url = do
    Right response <- simpleHTTP $ getRequest url
    let fasta = rspBody response
    let headers = rspHeaders response
    let location = lookupHeader HdrLocation headers
    case location of
        Nothing -> return $ parse fasta !! 0
        Just l  -> getUniprotFasta l

getProteins :: [String] -> IO [(String, String)]
getProteins uniprotIds = do
    let urls = map fastaUrl uniprotIds
    fastas <- mapM getUniprotFasta urls
    return . zip uniprotIds $ map snd fastas

motifToRegex :: String -> String
motifToRegex ""                 = ""
motifToRegex ('[':x:y:']':rest) = '(' : x : '|' : y : ')' : motifToRegex rest
motifToRegex ('{':x:'}':rest)   = "[^" ++ (x : ']' : motifToRegex rest)
motifToRegex (x:rest)           = x : motifToRegex rest

findAllMotifs :: String -> String -> [Int]
findAllMotifs motif protein 
    | index == -1 = []
    | otherwise   = index : findAllMotifs motif (drop (index + 1) protein) 
    where index = fst $ (protein =~ motifToRegex motif :: (Int, Int))

main = do
    content <- readFile idFileName
    prots <- getProteins $ lines content
    mapM_ print prots
    let matches = findAllMotifs glycosylation . snd $ prots !! 2
    putStrLn . fst $ prots !! 2
    print matches
    let indices = tail . scanl (+) 0 $ map (+1) matches
    print indices
    where idFileName = "mprot.txt"
          glycosylation = "N{P}[ST]{P}"
