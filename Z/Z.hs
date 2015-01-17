module Z where


import Data.List      (intercalate)

import Text.Printf    (printf)


varEx :: Key -> String -> ZExpr
varEx name value = ZExprs
    [ var name value
    , export name
    ]

var :: Key -> String -> ZExpr
var name value = ZVar name $ Var value


export :: Key -> ZExpr
export name = ZExport (printf "export %s" name)


alias, aliasS, aliasG :: Key -> String -> ZExpr
alias  = alias' AliasNormal
aliasS = alias' AliasSuffix
aliasG = alias' AliasGlobal

alias' :: AliasType -> Key -> String -> ZExpr
alias' atype name value =
    ZAlias name $ Alias value atype


raw :: String -> String -> ZExpr
raw = ZRaw


instance Show ZExpr where
    show (ZVar    { zKey = name
                  , zVar = Var value }) =
        printf "%s=%s" name value

    show (ZExport { zKey = name }) =
        printf "%s" name

    show (ZAlias  { zKey   = name
                  , zAlias = Alias value atype }) =
        printf "alias %s %s=%s"
            (show atype)
            name
            (quote value)

    show (ZRaw    { zKey   = name
                  , zValue = value }) =
        printf "%s %s" name value

    show (ZExprs  { zExprs = exprs }) =
        intercalate "\n" $ map show exprs


quote :: String -> String
quote = printf "'%s'"


instance Show AliasType where
    show atype = case atype of
        AliasNormal -> "  "
        AliasSuffix -> "-s"
        AliasGlobal -> "-g"


data ZExpr
    = ZVar
        { zKey    :: Key
        , zVar    :: Var
        }
    | ZExport
        { zKey    :: Key
        }
    | ZAlias
        { zKey    :: Key
        , zAlias  :: Alias
        }
    | ZRaw
        { zKey    :: String
        , zValue  :: String
        }
    | ZExprs
        { zExprs  :: [ZExpr]
        }


type Key  = String


data Var = Var
    { vValue  :: String
    } deriving (Show, Eq)


data Alias = Alias
    { aValue  :: String
    , aType   :: AliasType
    } deriving (Show, Eq)


data AliasType
    = AliasNormal
    | AliasSuffix
    | AliasGlobal
    deriving Eq
