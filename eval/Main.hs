import Data.List.Split (splitOn)
import System.Environment (getArgs)

main :: IO ()
main = do
  path:[] <- getArgs
  input <- readFile path
  let nStr:dna:gcStr:rest = lines input
  let n = read nStr :: Int
  let gcs = map read $ splitOn " " gcStr :: [Double]
  print n
  print dna
  print gcs
