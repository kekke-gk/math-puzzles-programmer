import Numeric

isPalindrome s = s == reverse s

radixConversions = [show, (`showOct` ""), (`showBin` "")]
isPalindromeAll n = all (isPalindrome . ($ n)) radixConversions

main = do
    print $ head $ filter isPalindromeAll [10..]