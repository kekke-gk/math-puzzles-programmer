cutbar m n cur
    | cur >= n = 0
    | otherwise = 1 + cutbar m n (cur + min m cur)

main = do
    print $ cutbar 3 20 1
    print $ cutbar 5 100 1
