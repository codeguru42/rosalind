-- This program is free software. It comes without any warranty, to
-- the extent permitted by applicable law. You can redistribute it
-- and/or modify it under the terms of the Do What The Fuck You Want
-- To Public License, Version 2, as published by Sam Hocevar. See
-- http://sam.zoy.org/wtfpl/COPYING for more details.

probAA = [0.5 , 0.5, 0.0 ]
probAa = [0.25, 0.5, 0.25]
probaa = [0.0 , 0.5, 0.5 ]

multProbs x = map (x*)
sumProbs = foldr (\[a, b, c] [a', b', c'] -> [a + a', b + b', c + c'])  [0.0, 0.0, 0.0]

pdf 1 probs = probs
pdf n probs = sumProbs $ zipWith multProbs probs pdf'
    where pdfAA = pdf (n-1) probAA
          pdfAa = pdf (n-1) probAa
          pdfaa = pdf (n-1) probaa
          pdf' = [pdfAA, pdfAa, pdfaa]

choose n k = product [n-k'+1..n] `div` product [1..k']
    where k' = min k (n - k)

binomPdf :: Double -> Integer -> Integer -> Double
binomPdf p n k = coeff * p**k' * (1 - p)**(n' - k')
    where n' = fromIntegral n
          k' = fromIntegral k
          coeff = fromIntegral (n `choose` k)

binomCdf :: Double -> Integer -> Integer -> Double
binomCdf p n k = sum $ map (binomPdf p n) [0..k-1]

main = do
    let p = binomCdf 0.25 4 1
    print $ p
    print $ (1 - p)
    let q = binomCdf 0.25 (2^6) 17
    print $ q
    print $ (1 - q)
