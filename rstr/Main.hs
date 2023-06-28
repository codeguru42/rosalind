import System.Environment (getArgs)

main :: IO ()
main = do
  nStr:xStr:dna:[] <- getArgs
  let n = read nStr :: Int
  let x = read xStr :: Float
  print n
  print x
  print dna
