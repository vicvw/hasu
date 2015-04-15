module Z where


import Data.List      (intercalate)

import Text.Printf    (printf)


varEx :: Key -> String -> Expr
varEx name value = Exprs
    [ var name value
    , export name
    ]

var :: Key -> String -> Expr
var name value = Var name $ Var' value


export :: Key -> Expr
export name = Export (printf "export %s" name)


alias, aliasS, aliasG :: Key -> String -> Expr
alias  = alias' AliasNormal
aliasS = alias' AliasSuffix
aliasG = alias' AliasGlobal

aliasS' cmd = Exprs . map (flip aliasS cmd)

alias' :: AliasType -> Key -> String -> Expr
alias' atype name value =
    Alias name $ Alias' value atype


func :: String -> [String] -> Expr
func = Function


raw :: String -> String -> Expr
raw = Raw


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
            (quote value)

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


quote :: String -> String
quote = printf "'%s'"

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
