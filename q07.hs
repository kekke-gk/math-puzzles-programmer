import Numeric
import Data.List

padding digit pad str
    | len > 0 = replicate len pad ++ str
    | otherwise = str
    where len = digit - length str
binStrings digit = map (padding digit '0' . (`showBin` "")) [0..2^digit-1]
palindromesInBin len
    | even len = map (\s -> reverse s ++ s) $ binStrings $ len `div` 2
    | otherwise = map (\s -> reverse (tail s) ++ s) $ binStrings $ 1 + len `div` 2
lengthInBin n = length $ showBin n ""
listDates d = map (show . fst . head . readBin) $ palindromesInBin $ lengthInBin (read d :: Int)
takeYear = take 4
takeMonth d = take 2 $ drop 4 d
takeDay d = reverse $ take 2 $ reverse d
isValidDate d1 d2 d = length d == 8 &&
    d1 <= d && d <= d2 &&
    "01" <= month && month <= "12" &&
    "01" <= day && day <= "31"
    where year = takeYear d
          month = takeMonth d
          day = takeDay d
validDates d1 d2 = sort $ filter (isValidDate d1 d2) $ listDates d2

main = do
    print $ validDates "19641010" "20200724"
