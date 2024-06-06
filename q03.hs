flipCards cards n
    | n > len = cards
    | otherwise = flipCards flipped (n+1)
    where len = length cards
          flags = take len $ cycle $ replicate n False ++ [True]
          flipped = zipWith (\card flag -> if flag then not card else card) cards flags

main = do
    let cards = replicate 100 False
    print $ [v | (v, flag) <- zip [1..] $ flipCards cards 1, not flag]
