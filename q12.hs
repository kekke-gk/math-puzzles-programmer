hasAllNum s = all ((`elem` s) . head . show) [0..9]

get10Digits n = take 10 $ filter (/='.') $ show n
get10Digits' n = take 10 $ tail $ dropWhile (/='.') $ show n

firstMatch f = fst $ head $ filter snd $ [(n, hasAllNum $ f $ sqrt n) | n <- [1..]]

main = do
    print $ firstMatch get10Digits
    print $ firstMatch get10Digits'
