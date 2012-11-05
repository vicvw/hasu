main = do
    print $ sumOfFibsUnder even 4000000

fibonacci :: Int -> Integer
fibonacci n = fibs !! n
    where fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

fibonacci' :: Integer -> Integer
fibonacci' n = fibs 0 1 0 (n - 1)
    where fibs x y start end
              | start == end = next
              | otherwise    = fibs next x (start + 1) end
              where next = x + y

sumOfFibsUnder :: (Integer -> Bool) -> Integer -> Integer
sumOfFibsUnder f x = sum . takeWhile (< x) . filter f . map fibonacci $ [1..]
