import Data.List.Split (splitOn)
import Rosalind (logProb)
import System.Environment (getArgs)

eval :: Int -> String -> [Double] -> [Double]
eval n dna gcs = map (\x -> fromIntegral (n - m + 1) * 10**x) lps
  where lps = map (logProb dna) gcs
        m = length dna

main :: IO ()
main = do
  path:[] <- getArgs
  input <- readFile path
  let nStr:dna:gcStr:_ = lines input
  let n = read nStr :: Int
  let gcs = map read $ splitOn " " gcStr :: [Double]
  let probs = eval n dna gcs
  putStrLn $ (unwords . map show) probs
