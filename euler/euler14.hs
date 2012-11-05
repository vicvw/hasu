import Data.List (maximumBy)

main = do
    putStrLn . show $ longestChainUnder 1000000

someSequence :: Integer -> [Integer]
someSequence 1 = [1]
someSequence x = x : someSequence (next x)
    where
        next x
            | even x = x `div` 2
            | odd x  = 3 * x + 1

longestChainUnder :: Integer -> Integer
longestChainUnder x = fst . maximumBy compareSnd $ map sequenceTuple [1..x-1]
    where
         compareSnd (_, n1) (_, n2) = compare n1 n2
         sequenceTuple x            = (x, length . someSequence $ x)
