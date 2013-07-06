import Data.Numbers.Primes -- again uber evil...


main = print $ sumOfPrimesBelow 2000000


sumOfPrimesBelow :: Integer -> Integer
sumOfPrimesBelow x = sum . takeWhile (< x) $ primes
