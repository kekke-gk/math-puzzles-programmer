collatz 1 = [1]
collatz n
    | even n = n : collatz (n `div` 2)
    | odd n = n : collatz (n * 3 + 1)

main = do
    print $ length $ filter (\x -> x `elem` collatz (x * 3 + 1)) $ filter even [1..10000]
