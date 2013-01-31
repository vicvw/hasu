-- not very efficient.

import Data.List      (maximumBy)
import Data.Ord       (comparing)
import Control.Arrow  ((&&&))


main = print $ longestChainUnder 1000000


longestChainUnder :: Integer -> Integer
longestChainUnder
    = fst
    . maximumBy (comparing snd)
    . map sequenceTuple
    . enumFromTo 1
    . (subtract 1)
    where
    sequenceTuple = id &&& length . someSequence


someSequence :: Integer -> [Integer]
someSequence 1 = [1]
someSequence x = x : someSequence (next x)
    where
    next x
        | even x = x `div` 2
        | odd x  = 3 * x + 1
