-- This program is free software. It comes without any warranty, to
-- the extent permitted by applicable law. You can redistribute it
-- and/or modify it under the terms of the Do What The Fuck You Want
-- To Public License, Version 2, as published by Sam Hocevar. See
-- http://sam.zoy.org/wtfpl/COPYING for more details.

import Network.HTTP

main = do
    response <- simpleHTTP $ getRequest url
    body <- getResponseBody response
    putStrLn body
    where baseUrl = "http://www.uniprot.org/uniprot/"
          uniprotId = "B5ZC00"
          url = baseUrl ++ uniprotId ++ ".fasta"
