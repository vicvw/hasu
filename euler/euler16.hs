import Data.Char

main = do
  putStrLn . show . sumOfDigits $ 2 ^ 1000

sumOfDigits :: Integer -> Int
sumOfDigits = sum . map digitToInt . show
