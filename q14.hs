import Data.Char

countries = ["Brazil", "Croatia", "Mexico", "Cameroon", "Spain", "Netherlands",
             "Chile", "Australia", "Colombia", "Greece", "Cote d'lvoire",
             "Japan", "Uruguay", "Costa Rica", "England", "Italy", "Switzerland",
             "Ecuador", "France", "Honduras", "Argentina", "Bosnia and Herzegovina",
             "Iran", "Nigeria", "germany", "Portugal", "Ghana", "USA",
             "Belgium", "Algeria", "Russia", "Korea Republic"]

search ls
    | null cs = (ls, length ls)
    | otherwise = foldr1 (\xs ys -> if snd xs > snd ys then xs else ys) $ map (search . (:ls)) cs
    where cs = filter (\x -> null ls || toLower (head x) == toLower (last $ head ls)) $ filter (`notElem` ls) countries

main = do
    print $ search []
