main = do
    print $ sumOfAllMultiplesUnder [3, 5] 1000

sumOfAllMultiplesUnder :: Integral a => [a] -> a -> a
sumOfAllMultiplesUnder list x = sum . filter (multiplesOf list) $ [1..x - 1]
    where multiplesOf list x = any (\m -> x `mod` m == 0) list
