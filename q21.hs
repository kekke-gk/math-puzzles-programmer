xor a b = not a && b || a && not b

tri' r n
    | n == 0 = True
    | n == r = True
    | otherwise = xor (triList !! (r-1) !! (n-1)) (triList !! (r-1) !! n)

tri 0 = [True]
tri r = map (tri' r) [0..r]

triList = map tri [0..]

main = do
    print $ (+1) $ length $ takeWhile (<2014) $ scanl1 (+) $ map (length . filter not) triList
