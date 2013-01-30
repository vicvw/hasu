module Main where


data KiType  = KiInteger Integer
             | KiDouble  Double
             | KiString  String
               deriving (Eq)
type Columns = [String]
type Rows    = [[KiType]]
type Table   = (Columns, Rows)


main = putStrLn . show
     $ table
  where
    table = (["ID", "Name", "Stupidity"]
            ,[[KiInteger 0, KiString "Klaus", KiString "High"]
             ,[KiInteger 1, KiString "S.P-J", KiString "Low"]])


instance Show KiType where
    show (KiInteger x) = show x
    show (KiDouble  x) = show x
    show (KiString  x) = show x
