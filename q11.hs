import Numeric

fib 0 = 1
fib 1 = 1
fib n = fibList !! (n - 2) + fibList !! (n - 1)

fibList = map fib [0..]

sumDigits n = sum $ map (fst . head . readDec . (:[])) $ show n

main = do
    print $ take 13 $ filter (\x -> x `mod` sumDigits x == 0) fibList
