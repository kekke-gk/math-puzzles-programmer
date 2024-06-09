c n m = product [n-m+1..n] `div` product [1..m]
count n = sum $ map (\x -> c (n-x+1) x) [0..n `div` 2]

main = do
    print $ count 30
