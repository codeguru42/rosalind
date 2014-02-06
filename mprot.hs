-- This program is free software. It comes without any warranty, to
-- the extent permitted by applicable law. You can redistribute it
-- and/or modify it under the terms of the Do What The Fuck You Want
-- To Public License, Version 2, as published by Sam Hocevar. See
-- http://sam.zoy.org/wtfpl/COPYING for more details.

import Data.Maybe (fromMaybe)
import Network.HTTP

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

main = do
    content <- readFile idFileName
    let uniprotIds = lines content
    let urls = map fastaUrl uniprotIds
    fastas <- mapM getUniprotFasta urls
    let prots = zip uniprotIds $ map snd fastas
    mapM_ print prots
    where idFileName = "mprot.txt"
