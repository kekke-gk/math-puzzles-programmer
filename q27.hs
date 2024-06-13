data Shift = Shift {
    x :: Int,
    y :: Int
} deriving (Show, Eq)

data Path = Path {
    start :: Shift,
    end :: Shift
} deriving (Show)

instance Eq Path where
    Path s e == Path s' e' = (s == s' && e == e') || (s == e' && s' == e)

unitShifts = map (uncurry Shift) [(1, 0), (0, 1), (-1, 0), (0, -1)]

add a b = Shift (x a + x b) (y a + y b)

dir (Shift 0 0) = -1
dir (Shift x 0)
    | x > 0 = 0
    | otherwise = 2
dir (Shift 0 y)
    | y > 0 = 1
    | otherwise = 3
dir s = -1

isTurnLeftOrStraight s s' =
    let d = dir s
        d' = dir s'
    in ((d + 1) `mod` 4) == d' || d == d'

isTurnRight s s' = not $ isTurnLeftOrStraight s s'

move w h shifts paths
    | x cur < 0 = []
    | y cur < 0 = []
    | x cur > w = []
    | y cur > h = []
    | length shifts >= 2 && isTurnRight (shifts !! 1) (shifts !! 0) = []
    | cur == Shift w h = [shifts]
    | otherwise = foldl1 (++) [move w h (s:shifts) (p:paths) | s <- unitShifts, let p = Path cur (add cur s), p `notElem` paths]
    where cur = foldl1 add shifts

main = do
    print $ length $ move 6 4 [Shift 0 0] []
