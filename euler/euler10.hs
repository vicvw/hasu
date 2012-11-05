import Data.Numbers.Primes

main = do
    print $ sumOfPrimesBelow 2000000

sumOfPrimesBelow :: Integer -> Integer
sumOfPrimesBelow x = sum . takeWhile (< x) $ primes
