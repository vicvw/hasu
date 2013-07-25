module Z
    ( alias
    , aliasS
    , aliasG
    , depsExist
    , export
    , invalidOrder
    , isValid
    , missingDeps
    , validOrder
    , var
    , varApp
    , ZShellExpr
    ) where


import Control.Arrow  ((&&&))

import Data.List      (intercalate, sort)
import Text.Printf    (printf)

-- import Debug.Trace    (traceShow)


instance Eq ZShellExpr where
    ZVariable name1 deps1 _ ==
        ZVariable name2 deps2 _ =
        name1 == name2 &&
        deps1 == deps2

    ZExport name1 deps1 ==
        ZExport name2 deps2 =
        name1 == name2 &&
        deps1 == deps2

    ZAlias name1 deps1 _ ==
        ZAlias name2 deps2 _ =
        name1 == name2 &&
        deps1 == deps2

    _ == _ = False


-- TODO: ...
-- instance Ord ZShellExpr where
--     -- (ZVariable {}) `compare`
--     --     (ZAlias {}) = traceShow "v < a" LT

--     (ZVariable name1 deps1 _) `compare`
--         (ZVariable name2 deps2 _) =
--         (name1, deps1) `compareBoth` (name2, deps2)

--     (ZAlias name1 deps1 _) `compare`
--         (ZAlias name2 deps2 _) =
--         (name1, deps1) `compareBoth` (name2, deps2)

--     (ZVariable name1 deps1 _) `compare`
--         (ZAlias name2 deps2 _) =
--         (name1, deps1) `compareBoth` (name2, deps2)

--     (ZAlias name1 deps1 _) `compare`
--         (ZVariable name2 deps2 _) =
--         (name1, deps1) `compareBoth` (name2, deps2)

--     -- _ `compare` _ = traceShow "else" LT


-- compareBoth (name1, deps1) (name2, deps2) =
--     if name1 `elem` deps2
--     then traceShow "n1 `elem` d2" LT

--     else if name1 `elem` deps2
--         then traceShow "n2 `elem` d1" GT
--         else
--             traceShow "else" EQ
--             -- traceShow "length" $ (length deps1, name1) `compare`
--             --     (length deps2, name2)


var :: Key -> String -> Deps -> ZShellExpr
var name value deps = ZVariable name deps $
    Variable value

varApp :: Key -> String -> Deps -> ZShellExpr
varApp name value = var name (printf "$%s:%s" name value)


export :: Key -> ZShellExpr
export name = ZExport (printf "export %s" name) [name]


alias, aliasS, aliasG :: Key -> String -> Deps -> ZShellExpr
alias  = alias' AliasNormal
aliasS = alias' AliasSuffix
aliasG = alias' AliasGlobal

alias' :: AliasType -> Key -> String -> Deps -> ZShellExpr
alias' atype name value deps = ZAlias name deps $
    Alias value atype


isValid :: [ZShellExpr] -> Bool
isValid conf = all ($ conf) [depsExist, validOrder]


depsExist :: [ZShellExpr] -> Bool
depsExist conf = flip all conf $
    all (`elem` map zName conf) . zDeps


validOrder :: [ZShellExpr] -> Bool
validOrder conf = flip all conf $
    \e -> not . any ((zName e `elem`) . zDeps) $
                    elemsBefore e

    where
    elemsBefore e = takeWhile (/= e) conf


missingDeps :: [ZShellExpr] -> [(Key, Deps)]
missingDeps conf
    = filter (not . null . snd)
    . map (zName &&&
           filter (not . (`elem` map zName conf)) .
                  zDeps)
    $ conf


invalidOrder :: [ZShellExpr] -> (ZShellExpr, [ZShellExpr])
invalidOrder conf
    = (,) culprit
    . filter ((zName culprit `elem`) . zDeps) $
             elemsBefore culprit

    where
    culprit = head $ filter (\e -> any ((zName e `elem`) . zDeps) $ elemsBefore e) conf
    elemsBefore e = takeWhile (/= e) conf


instance Show ZShellExpr where
    show expr = case expr of
        ZVariable { zName      = vname
                  , zDeps      = vdeps
                  , zVariable  = Variable vvalue
                  } ->
            printf "%s=%s%s"
                vname
                (quote vvalue)
                $ comment vdeps


        ZExport { zName      = vname
                , zDeps      = vdeps
                } ->
            printf "%s%s"
                vname
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


-- type ZShellConf = [ZShellExpr]


data ZShellExpr
    = ZVariable
          { zName      :: Key
          , zDeps      :: Deps
          , zVariable  :: Variable
          }
    | ZExport
          { zName      :: Key
          , zDeps      :: Deps
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


-- (-->) :: a -> b -> (a, b)
-- (-->) = (,)
