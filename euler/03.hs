import Control.Arrow ((&&&))


main = print $ biggestPrimeFactor 600851475143


divides :: Integer -> Integer -> Bool
divides x = (== 0) . mod x


nextPrimes :: Integer -> [Integer] -> [Integer]
nextPrimes 0 = id
nextPrimes n = nextPrimes (n - 1) . nextPrime


biggestPrimeFactor :: Integer -> Integer
biggestPrimeFactor x
    | x < 2     = 0
    | otherwise = biggestPrimeFactor' x []
    where
    biggestPrimeFactor' x list
        | x == next = x
        | otherwise = uncurry biggestPrimeFactor'
                    $ if x `divides` next
                      then (x `div` next, list)
                      else (x, next:list)
        where
        next = head $ nextPrime list


nextPrime :: [Integer] -> [Integer]
nextPrime []       = [2]
nextPrime l@(p:ps) = (head . dropWhile (`dividesAny` l) $ [p+1..]) : l
    where
    dividesAny x = any (divides x) . reverse


biggestPrimeFactorAsTupel :: Integer -> (Integer, Integer)
biggestPrimeFactorAsTupel = id &&& biggestPrimeFactor


biggestPrimeFactors :: Integer -> [(Integer, Integer)]
biggestPrimeFactors = map biggestPrimeFactorAsTupel . enumFromTo 2 . (+ 1)
