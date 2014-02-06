-- This program is free software. It comes without any warranty, to
-- the extent permitted by applicable law. You can redistribute it
-- and/or modify it under the terms of the Do What The Fuck You Want
-- To Public License, Version 2, as published by Sam Hocevar. See
-- http://sam.zoy.org/wtfpl/COPYING for more details.

import Network.HTTP

import Rosalind

getUniprotFasta :: String -> IO (String, String)
getUniprotFasta uniprotId = do
    response <- simpleHTTP $ getRequest url
    fasta <- getResponseBody response
    return $ parse fasta !! 0
    where baseUrl = "http://www.uniprot.org/uniprot/"
          url = baseUrl ++ uniprotId ++ ".fasta"

main = do
    fasta <- getUniprotFasta uniprotId
    print fasta
    where uniprotId = "B5ZC00"
