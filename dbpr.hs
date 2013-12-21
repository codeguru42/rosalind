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
    let parsedGos = map (splitOn "; ") gos
    let bios = concat . filter (not . null) $ map (filter ("P" `isPrefixOf`)) parsedGos
    let parsedBios = map (splitOn ":") bios
    mapM_ putStrLn $ map (!! 1) parsedBios
    where baseUrl = "http://www.uniprot.org/uniprot/"
          uniprotId = "P21923"
          url = baseUrl ++ uniprotId ++ ".txt"
