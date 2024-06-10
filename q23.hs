search n c
    | c == 0 = 0
    | n == 0 = 1
    | otherwise = sum [searchList !! (n-1) !! (c+m) | m <- [1,-1]]

searchList = [[search n c | c <- [0..]] | n <- [0..]]

main = do
    print $ search 24 10
