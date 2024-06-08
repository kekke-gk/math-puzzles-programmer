boy = 20
girl = 10

queue b g
    | b == 0 && g == 0 = 1
    | b == boy && g == girl = b' + g'
    | b == g = 0
    | boy - b == girl - g = 0
    | b == 0 = g'
    | g == 0 = b'
    | otherwise = b' + g'
    where b' = queueList !! g !! (b - 1)
          g' = queueList !! (g - 1) !! b

queueList = [[queue b g | b <- [0..boy]] | g <- [0..girl]]

main = do
    print $ queue boy girl
