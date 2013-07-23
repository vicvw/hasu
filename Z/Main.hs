module Main (main) where


import Control.Arrow  ((&&&))
import Data.List      (intercalate, sort)
import Text.Printf    (printf)

import Debug.Trace    (traceShow)


main :: IO ()
main = if isValid conf
    then mapM_ print $ sort conf
    else mapM_ print $ missingDeps conf

    where
    conf =
        [ alias "s" "sudo"
              ["HOME"]
        , aliasS "xz" "pm -U"
              ["pm"]
        , alias "pm" "s pacman"
              ["s", "HOME"]
        , var "HOME" "$HOME:/etc/hell"
              []
        ]


instance Eq ZShellExpr where
    (ZVariable name1 deps1 _) ==
        (ZVariable name2 deps2 _) =
        name1 == name2 &&
        deps1 == deps2

    (ZAlias name1 deps1 _) ==
        (ZAlias name2 deps2 _) =
        name1 == name2 &&
        deps1 == deps2

    _ == _ = False


-- TODO: ...
instance Ord ZShellExpr where
    -- (ZVariable {}) `compare`
    --     (ZAlias {}) = traceShow "v < a" LT

    (ZVariable name1 deps1 _) `compare`
        (ZVariable name2 deps2 _) =
        (name1, deps1) `compareBoth` (name2, deps2)

    (ZAlias name1 deps1 _) `compare`
        (ZAlias name2 deps2 _) =
        (name1, deps1) `compareBoth` (name2, deps2)

    (ZVariable name1 deps1 _) `compare`
        (ZAlias name2 deps2 _) =
        (name1, deps1) `compareBoth` (name2, deps2)

    (ZAlias name1 deps1 _) `compare`
        (ZVariable name2 deps2 _) =
        (name1, deps1) `compareBoth` (name2, deps2)

    -- _ `compare` _ = traceShow "else" LT


compareBoth (name1, deps1) (name2, deps2) =
    if name1 `elem` deps2
    then traceShow "n1 `elem` d2" LT

    else if name1 `elem` deps2
        then traceShow "n2 `elem` d1" GT
        else
            traceShow "else" EQ
            -- traceShow "length" $ (length deps1, name1) `compare`
            --     (length deps2, name2)


var :: Key -> String -> Deps -> ZShellExpr
var name value deps = ZVariable name deps $
    Variable value


alias, aliasS, aliasG :: Key -> String -> Deps -> ZShellExpr
alias  = alias' AliasNormal
aliasS = alias' AliasSuffix
aliasG = alias' AliasGlobal

alias' :: AliasType -> Key -> String -> Deps -> ZShellExpr
alias' atype name value deps = ZAlias name deps $
    Alias value atype


isValid :: ZShellConf -> Bool
isValid conf = flip all conf $
    all (`elem` map zName conf) . zDeps


missingDeps :: ZShellConf -> [(Key, Deps)]
missingDeps conf
    = filter (not . null . snd)
    . map (zName &&&
           filter (not . (`elem` map zName conf))
                 . zDeps)
    $ conf


instance Show ZShellExpr where
    show expr = case expr of
        ZVariable { zName      = vname
                  , zDeps      = vdeps
                  , zVariable  = Variable vvalue
                  } ->
            printf "%s=%s%s"
                vname
                vvalue
                $ comment vdeps

        ZAlias { zName  = aname
               , zDeps  = adeps
               , zAlias = Alias avalue atype
               } ->
            printf "alias %s %s=%s%s"
                (show atype)
                aname
                (quote avalue)
                $ comment adeps

        where
        comment []   = ""
        comment deps = printf "\t\t# %s"
                     . intercalate ", "
                     $ sort deps

        quote string = concat ["'", string, "'"]


instance Show AliasType where
    show atype = case atype of
        AliasNormal -> "  "
        AliasSuffix -> "-s"
        AliasGlobal -> "-g"


type ZShellConf = [ZShellExpr]


data ZShellExpr
    = ZVariable
          { zName      :: Key
          , zDeps      :: Deps
          , zVariable  :: Variable
          }
    | ZAlias
          { zName      :: Key
          , zDeps      :: Deps
          , zAlias     :: Alias
          }


type Key  = String
type Deps = [Key]


data Variable = Variable
    { vValue  :: String
    } deriving (Show, Eq)


data Alias = Alias
    { aValue  :: String
    , aType   :: AliasType
    } deriving Show


data AliasType
    = AliasNormal
    | AliasSuffix
    | AliasGlobal


(-->) :: a -> b -> (a, b)
(-->) = (,)
