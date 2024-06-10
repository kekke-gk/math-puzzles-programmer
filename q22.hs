pairs n
    | n <= 2 = 1
    | otherwise = sum $ map (\x -> pairs x * pairs (n-2-x)) [0,2..n-2]

main = do
    print $ pairs 16
