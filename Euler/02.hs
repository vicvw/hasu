main = print $ sumOfFibsUnder even 4000000


sumOfFibsUnder :: (Integer -> Bool) -> Integer -> Integer
sumOfFibsUnder f x
    = sum
    . takeWhile (< x)
    . filter f
    . map fibonacci
    $ [1..]


fibonacci :: Integer -> Integer
fibonacci = (fibs !!) . fromIntegral
    where
    fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
