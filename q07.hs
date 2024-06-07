import Numeric
import Data.Time.Calendar

day2str d = filter (/='-') $ show d
str2int = fst . head . readDec
int2bin = (`showBin` "")
isPalindrome s = s == reverse s
isValidDate d = isPalindrome $ int2bin $ str2int $ day2str d

main = do
    print $ filter isValidDate [fromGregorian 1964 10 10..fromGregorian 2020 7 24]
