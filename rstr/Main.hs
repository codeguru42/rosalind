import System.Environment (getArgs)

import Rosalind (logProb)

rstr :: Int -> Double -> String -> Double
rstr n x dna = 1 - lp' ^ n
  where lp = logProb dna x
        lp' = 1 - 10.0 ** lp

main :: IO ()
main = do
  nStr:xStr:dna:[] <- getArgs
  let n = read nStr :: Int
  let x = read xStr :: Double
  print $ rstr n x dna
