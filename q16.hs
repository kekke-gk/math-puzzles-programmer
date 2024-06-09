main = do
    print $ length [(a, b, c) | a <- [1..round(500/4)], b <- [1..a], c <- [1..b], a^2 == b^2 + c^2, gcd b c == 1]
