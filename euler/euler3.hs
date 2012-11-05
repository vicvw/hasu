main = do
    print $ biggestPrimeFactor 600851475143
    -- print $ biggestPrimeFactors 100
    -- print $ biggestPrimeFactorAsTupel 1000001
    -- print $ head $ nextPrimes 2500 []

modp :: Integral a => a -> a -> Bool
modp x p = mod x p == 0

nextPrime :: Integral a => [a] -> [a]
nextPrime []   = [2]
nextPrime list = (head $ dropWhile someDevides [head list + 1..]) : list
                 -- where someDevides x = or . map (modp x) . reverse $ list
                 where someDevides x = any (modp x) . reverse $ list

nextPrimes :: Integral a => a -> [a] -> [a]
nextPrimes 0 list = list
nextPrimes n list = nextPrimes (n - 1) $ nextPrime list

biggestPrimeFactor :: Integral a => a -> a
biggestPrimeFactor x
    | x < 2     = 0
    | otherwise = biggestPrimeFactor' x []
    where biggestPrimeFactor' x list
              | x == next = x
              | otherwise = if x `modp` next
                               then biggestPrimeFactor' (x `div` next) list
                               else biggestPrimeFactor' x $ next:list
              where next = head $ nextPrime list

biggestPrimeFactorAsTupel :: Integral a => a -> (a, a)
biggestPrimeFactorAsTupel x = (x, biggestPrimeFactor x)

biggestPrimeFactors :: Integral a => a -> [(a, a)]
biggestPrimeFactors n = map biggestPrimeFactorAsTupel [2..n + 1]
