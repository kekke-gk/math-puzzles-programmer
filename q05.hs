changes amount limit coins ls
    | remain == 0 = 1 
    | length ls == limit = 0
    | otherwise = sum $ map (changes amount limit coins . (:ls)) $ filter (<=minimum (remain : ls)) coins
    where remain = amount - sum ls

main = do
    print $ changes 100 15 [50, 10, 5, 1] []
