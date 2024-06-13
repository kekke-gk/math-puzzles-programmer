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

-- instance Ord Path where
--     compare (Path s e) (Path s' e')
--         | (s == s' && e == e') || (s == e' && s' == e) = EQ
--         | otherwise = compare s s' <> compare e e'

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

-- isTurnRight s s' = dir s == ((dir s' + 1) `mod` 4)
isTurnRight s s' = not $ isTurnLeftOrStraight s s'

shifts2paths = scanl (\acc s -> Path (end acc) (add (end acc) s)) $ Path (Shift 0 0) (Shift 0 0)

-- move w h shifts
--     | x cur < 0 = 0
--     | y cur < 0 = 0
--     | x cur > w = 0
--     | y cur > h = 0
--     | length shifts >= 2 && isTurnRight (shifts !! 1) (shifts !! 0) = 0
--     | length shifts >= 2 && tookSamePath (shifts2paths shifts) = 0
--     | cur == Shift w h = 1
--     | otherwise = sum $ map (move w h . (:shifts)) unitShifts
--     where cur = foldl add (Shift 0 0) shifts
--     -- where cur = foldl1 add shifts

-- move w h shifts
--     | x cur < 0 = []
--     | y cur < 0 = []
--     | x cur > w = []
--     | y cur > h = []
--     | length shifts >= 2 && isTurnRight (shifts !! 1) (shifts !! 0) = []
--     | length shifts >= 2 && tookSamePath (shifts2paths shifts) = []
--     | cur == Shift w h = [shifts]
--     | otherwise = foldl1 (++) $ map (move w h . (:shifts)) unitShifts
--     -- where cur = foldl add (Shift 0 0) shifts
--     where cur = foldl1 add shifts

move w h shifts paths
    | x cur < 0 = []
    | y cur < 0 = []
    | x cur > w = []
    | y cur > h = []
    | length shifts >= 2 && isTurnRight (shifts !! 1) (shifts !! 0) = []
    | cur == Shift w h = [shifts]
    | otherwise = foldl1 (++) [move w h (s:shifts) (newPath:paths) | s <- unitShifts, let newPath = Path cur (add cur s), newPath `notElem` paths]
    where cur = foldl1 add shifts

main = do
    print $ length $ move 6 4 [Shift 0 0] []
