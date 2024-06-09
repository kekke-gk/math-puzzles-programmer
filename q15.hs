search x
    | x < 0 = 0
    | x == 0 = 1
    | otherwise = sum $ [(search . (x-)) (a + b) | a <- [1..4], b <- [1..4]]

main = do
    print $ search 10
