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
findAllMotifs motif = tail . scanl (+) 0 . map (+1) . findAllMotifs' motif
    where findAllMotifs' motif protein 
            | index == -1 = []
            | otherwise   = index : findAllMotifs' motif (drop (index + 1) protein) 
            where index = fst $ (protein =~ motifToRegex motif :: (Int, Int))

printMotifs :: [(String, [Int])] -> [IO ()]
printMotifs []     = [putStrLn ""]
printMotifs (x:xs) = printMotifs' x ++ printMotifs xs
    where printMotifs' (id, matches) = [putStrLn id, putStrLn $ format matches]

main = do
    content <- readFile idFileName
    prots <- getProteins $ lines content
    let allMatches = map (findAllMotifs glycosylation) (map snd prots)
    let motifs = filter (not . null . snd) $ zip (map fst prots) allMatches
    sequence_ $ printMotifs motifs
    where idFileName = "mprot.txt"
          glycosylation = "N{P}[ST]{P}"
