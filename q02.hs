import Control.Monad (replicateM)
import Data.List.Split
import Numeric

nums = [1000..9999]
operators = "* "

interleave [] ys = ys
interleave xs [] = xs
interleave (x:xs) (y:ys) = x : y : interleave xs ys

operatorReplicates = filter (/="   ") $ replicateM 3 operators
-- ["***","** ","* *","*  "," **"," * ","  *"]

str2int = fst . head . readDec
splitByOpe n ope = map str2int $ splitOn "*" $ filter (/=' ') $ interleave (reverse $ show n) ope
-- 1234 " * " = [12,34]

main = do
    print $ filter (\(n, l) -> n == product l) $ [(n, splitByOpe n ope) | n <- nums, ope <- operatorReplicates]
