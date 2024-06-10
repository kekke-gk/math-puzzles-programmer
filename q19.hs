import Data.List
primes = [2, 3, 5, 7, 11, 13]

maxProd [a,b,c,d,e,f] = maximum [a*a,a*b,b*c,c*d,d*e,e*f,f*f]

main = do
    print $ minimum $ map maxProd $ permutations primes
