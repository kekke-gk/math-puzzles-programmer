import Data.List
import Control.Monad

nums = [1,14,14,4,11,7,6,9,8,10,10,5,13,2,3,15]

powerset = filterM (const [True, False])

counts = map count . group . sort
    where count xs = (head xs, length xs)

main = do
    print $ last $ sortOn snd $ counts $ map sum $ powerset nums
