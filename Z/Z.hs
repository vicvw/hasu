module Z where


import Data.List    (intercalate)
import Text.Printf  (printf)


raw' :: [(String, String)] -> Expr
raw' = exprs $ uncurry raw

raw :: String -> String -> Expr
raw = Raw


var' :: [(String, String)] -> Expr
var' = exprs $ uncurry var

var :: String -> String -> Expr
var = Var


varEx :: String -> String -> Expr
varEx n v = Exprs [ var n v, Export n ]


alias' :: [(String, String)] -> Expr
alias' = exprs $ uncurry alias

aliasS'' :: [(String, [String])] -> Expr
aliasS'' = exprs $ uncurry aliasS'

aliasS' :: String -> [String] -> Expr
aliasS' = exprs . flip aliasS

alias, aliasS, aliasG :: String -> String -> Expr
alias  = alias_ ANormal
aliasS = alias_ ASuffix
aliasG = alias_ AGlobal

alias_ :: AType -> String -> String -> Expr
alias_ = flip $ flip . Alias


func' :: [(String, [String])] -> Expr
func' = exprs $ uncurry func

func :: String -> [String] -> Expr
func = Func


exprs :: (a -> Expr) -> [a] -> Expr
exprs = (Exprs .) . map


instance Show Expr where
    show (Raw    n v  ) = unwords [n, v]
    show (Var    n v  ) = printf "%s=%s" n v
    show (Export n    ) = unwords ["export", n]
    show (Alias  n v t) = printf "alias %s %s=%s" (show t) n (q' v)
    show (Func   n b  ) = printf "function %s {\n%s}" n . unlines $ map (indent 1) b
    show (Exprs  es   ) = intercalate "\n" $ map show es

instance Show AType where
    show ANormal = "  "
    show ASuffix = "-s"
    show AGlobal = "-g"


q         = wrap "\""
q'        = wrap "'"
wrap w s  = concat [w, s, w]
indent n  = (concat (replicate n "  ") ++)


data Expr
    = Raw     String String
    | Var     String String
    | Export  String
    | Alias   String String AType
    | Func    String [String]
    | Exprs   [Expr]

data AType
    = ANormal
    | ASuffix
    | AGlobal
