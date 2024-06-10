deleteAt idx xs = l ++ r
    where (l, _:r) = splitAt idx xs

strikeout ls = strikeout1 ls ++ strikeout2 ls
strikeout1 ls = map (`deleteAt` ls) [0..length ls-1]
strikeout2 ls = [filter (\x -> x /= n && x /= n2) ls | n <- [0..7], let n2 = (n+1) `mod` 8, all (`elem` ls) [n, n2]]


search ls
    | null ls = 1
    | otherwise = sum $ map search $ strikeout ls

main = do
    print $ search [0..8]
