isSquared x = let s = sqrt $ fromIntegral x in floor s == ceiling s

search n ls
    | null xs = isSquared (head ls + last ls) && (length ls == n)
    | otherwise = any (search n . (:ls)) xs
    where xs = [x | x <- [1..n], x `notElem` ls, isSquared (x + head ls)]

main = do
    print $ head $ [n | n <- [1..], search n [1]]
