clubs = [(11000, 40), (8000, 30), (400, 24), (800, 20), (900, 14),
         (1800, 16), (1000, 15), (7000, 40), (100, 10), (300, 12)]

search clubIdxs n
    | null idxs = sum [fst $ clubs !! i | i <- clubIdxs]
    | otherwise = maximum [search (i : clubIdxs) (n - snd (clubs !! i))  | i <- idxs]
    where idxs = [i | i <- [0..length clubs - 1], i `notElem` clubIdxs, n >= snd (clubs !! i)]

main = do
    print $ search [] 150
