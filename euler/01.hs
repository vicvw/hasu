main = print $ sumOfMultiplesUnder [3, 5] 1000


sumOfMultiplesUnder list x
    = sum
    . takeWhile (< x)
    . filter (`dividesAny` list)
    $ [1..]

    where
    dividesAny x  = any (divides x)
    divides x     = (== 0) . mod x
