cat 0 = 1
cat 1 = 1
cat n = sum [cat (k-1) * cat (n-k) | k <- [1..n]]

main = do
    mapM_ putStrLn [show $ cat n | n <- [0..10]]
