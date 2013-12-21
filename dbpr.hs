-- This program is free software. It comes without any warranty, to
-- the extent permitted by applicable law. You can redistribute it
-- and/or modify it under the terms of the Do What The Fuck You Want
-- To Public License, Version 2, as published by Sam Hocevar. See
-- http://sam.zoy.org/wtfpl/COPYING for more details.

import Network.HTTP
import Data.List (isPrefixOf)
import Data.List.Split (splitOn)

main = do
    response <- simpleHTTP $ getRequest url
    body <- getResponseBody response
    let drs = filter ("DR" `isPrefixOf`) $ lines body
    let gos = filter ("GO" `isPrefixOf`) $ map (drop 5) drs
    let parsed = map (splitOn "; ") gos
    mapM_ print parsed
    where baseUrl = "http://www.uniprot.org/uniprot/"
          uniprotId = "H3SRW3"
          url = baseUrl ++ uniprotId ++ ".txt"
