import Data.List

isValid xs = r /= 0 &&
             w /= 0 &&
             t /= 0 &&
             s /= 0 &&
             read' + write + talk == skill
    where [r,e,a,d,w,i,t,l,k,s] = xs
          read' = list2int [r,e,a,d]
          write = list2int [w,r,i,t,e]
          talk = list2int [t,a,l,k]
          skill = list2int [s,k,i,l,l]

list2int xs = sum $ zipWith (\x n -> x * 10 ^ n) xs (reverse [0..length xs-1])

main = do
    print $ length $ filter id $ map isValid $ permutations [0..9]
