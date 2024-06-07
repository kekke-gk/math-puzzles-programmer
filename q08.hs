data Point = Point {
    x :: Int,
    y :: Int
} deriving (Show, Eq)

add a b = Point (x a + x b) (y a + y b)

dPoints = [Point x y | x <- [-1,1], y <- [-1,1]]
moveRobot n route
    | length route == n + 1 = 1
    | otherwise = sum $ map (moveRobot n . (:route)) $ filter (`notElem` route) $ map (add $ head route) dPoints

main = do
    print $ moveRobot 12 [Point 0 0]
