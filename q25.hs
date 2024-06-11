data Line = Line {
    l :: Int,
    r :: Int
} deriving (Show, Eq)

isCross a b = ((l a > l b) && (r b > r a)) || ((l a < l b) && (r b < r a))

comb 0 xs = [[]]
comb _ [] = []
comb n (x:xs) = [x:y | y <- comb (n-1) xs] ++ comb n xs

applyFuncToList2 f [a,b] = f a b

countCross lines = length $ filter id $ map (applyFuncToList2 isCross) $ comb 2 lines

search ls rs lines
    | ll == 6 && rl == 5 = countCross $ Line (head ls) 0:lines
    | ll > rl = maximum [search ls (x:rs) (line:lines) | x <- [1..5], x `notElem` rs, let line = Line (head ls) x]
    | otherwise = maximum [search (x:ls) rs (line:lines) | x <- [0..5], x `notElem` ls, let line = Line x (head rs)]
    where ll = length ls
          rl = length rs

main = do
    print $ search [0] [] []
