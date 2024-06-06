main = do
    let n = 100
    print $ map fst $ filter (even . snd) $ zip [1 ..] $ take n $
        foldr1 (zipWith (+)) $ map (\n -> cycle $ replicate n 0 ++ [1]) [1 .. n]
