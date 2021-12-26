import Test.HUnit


testCat n expected = ("test cat " ++ show n) ~: expected ~=? cat n
tests = test $ map (\(n, expected) -> testCat n expected) $ zip [0..20] allExpected
        where
            allExpected =
                [
                    1, 1, 2, 5, 14, 42, 132, 429, 1430, 4862, 16796, 58786, 208012, 742900, 2674440,
                    9694845, 35357670, 129644790, 477638700, 1767263190, 6564120420, 24466267020,
                    91482563640, 343059613650, 1289904147324, 4861946401452, 18367353072152,
                    69533550916004, 263747951750360, 1002242216651368, 3814986502092304
                ]

cat 0 = 1
cat 1 = 1
cat n = sum [cat (k-1) * cat (n-k) | k <- [1..n]]

main = do
    runTestTT tests
