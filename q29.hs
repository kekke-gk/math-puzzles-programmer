import Data.Foldable (minimumBy)
import Data.Ord (comparing)
import Text.Printf (printf)

series a b = a + b
parallel a b = recip $ sum $ map recip [a, b]

resistors n
    | n == 1 = [1]
    | otherwise = [series 1, parallel 1] <*> resistors (n - 1)

goldenNum = (1 + sqrt 5) / 2

nearestValue target = minimumBy $ comparing $ abs . (target-)

formatNumber :: Double -> String
formatNumber = printf "%.10f"

main = do
    print $ formatNumber $ nearestValue goldenNum $ resistors 10
