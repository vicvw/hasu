module Z where


import Data.List      (intercalate)

import Text.Printf    (printf)


varEx :: Key -> String -> Expr
varEx name value = Exprs
    [ var name value
    , export name
    ]

export :: Key -> Expr
export name = Export (printf "export %s" name)


var' :: [(String, String)] -> Expr
var' = exprs $ uncurry var

var :: Key -> String -> Expr
var name value = Var name $ Var' value


alias' :: [(String, String)] -> Expr
alias' = exprs $ uncurry alias

-- aliasS'' :: String -> [String] -> Expr
aliasS'' = exprs $ uncurry aliasS'

aliasS' :: String -> [String] -> Expr
aliasS' = exprs . flip aliasS

alias, aliasS, aliasG :: Key -> String -> Expr
alias  = alias_ AliasNormal
aliasS = alias_ AliasSuffix
aliasG = alias_ AliasGlobal

alias_ :: AliasType -> Key -> String -> Expr
alias_ atype name value =
    Alias name $ Alias' value atype


func' :: [(String, [String])] -> Expr
func' = exprs $ uncurry func

func :: String -> [String] -> Expr
func = Function


raw' :: [(String, String)] -> Expr
raw' = exprs $ uncurry raw

raw :: String -> String -> Expr
raw = Raw


exprs f = Exprs . map f


instance Show Expr where
    show Var      { zKey = name
                  , zVar = Var' value } =
        printf "%s=%s" name value

    show Export   { zKey = name } =
        printf "%s" name

    show Alias    { zKey   = name
                  , zAlias = Alias' value atype } =
        printf "alias %s %s=%s"
            (show atype)
            name
            (q' value)

    show Function { zKey  = name
                  , zFunc = func } =
        printf "function %s {\n%s}"
            name
          . unlines $ map (indent 1) func

    show Raw      { zKey   = name
                  , zValue = value } =
        printf "%s %s" name value

    show Exprs    { zExprs = exprs } =
        intercalate "\n" $ map show exprs


q, q' :: String -> String
q         = wrap "\""
q'        = wrap "'"
wrap w s  = concat [w, s, w]

indent :: Int -> String -> String
indent n = (concat (replicate n "  ") ++)


instance Show AliasType where
    show atype = case atype of
        AliasNormal -> "  "
        AliasSuffix -> "-s"
        AliasGlobal -> "-g"


data Expr
    = Var
        { zKey    :: Key
        , zVar    :: Var'
        }
    | Export
        { zKey    :: Key
        }
    | Alias
        { zKey    :: Key
        , zAlias  :: Alias'
        }
    | Function
        { zKey    :: Key
        , zFunc   :: [String]
        }
    | Raw
        { zKey    :: String
        , zValue  :: String
        }
    | Exprs
        { zExprs  :: [Expr]
        }


type Key  = String


data Var' = Var'
    { vValue  :: String
    } deriving (Show, Eq)


data Alias' = Alias'
    { aValue  :: String
    , aType   :: AliasType
    } deriving (Show, Eq)


data AliasType
    = AliasNormal
    | AliasSuffix
    | AliasGlobal
    deriving Eq
