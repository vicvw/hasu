main = print . sumOfDigits $ 2 ^ 1000


sumOfDigits :: Integer -> Integer
sumOfDigits = sum . map (read . return) . show
