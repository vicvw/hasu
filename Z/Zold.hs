module Z
    where
--     ( alias
--     , aliasS
--     , aliasG
--     , depsExist
--     , export
--     , invalidOrder
--     , isValid
--     , missingDeps
--     , validOrder
--     , var
--     , varApp
--     , ZExpr
--     ) where


import Control.Arrow  (first, (&&&), (***))

import Data.Function  (on)
import Data.List      (elemIndex, find, groupBy, intercalate, intersect, nub, sort, sortBy, (\\))
import Data.Maybe     (fromJust)
import Data.Ord       (comparing)

import Text.Printf    (printf)


topSort :: [ZExpr] -> [Key]
topSort xs
    | null $ cycleDetect xs = foldl putBefore [] xs
    | otherwise             = error $ "cycle: " ++ show (cycleDetect xs)


-- putBefore :: [ZExpr] -> ZExpr -> [ZExpr]
putBefore :: [Key] -> ZExpr -> [Key]
putBefore xs x = nub $ case elemIndex (zKey x) xs of
    Just i  -> uncurry (++) . first (++ zDeps x) $ splitAt i xs
    _       -> xs ++ zDeps x ++ [zKey x]


cycleDetect :: [ZExpr] -> [[Key]]
cycleDetect xs
    = filter ((> 1) . length)
    . map (\[x1, x2] -> ([zKey x1] `intersect` zDeps x2) ++ ([zKey x2] `intersect` zDeps x1))
    . combinations 2 $ xs


combinations :: Integer -> [ZExpr] -> [[ZExpr]]
combinations 0 _       = [[]]
combinations _ []      = []
combinations k (x:xs)  = map (x :) (combinations (k - 1) xs)
                      ++ combinations k xs


var :: Key -> String -> Deps -> ZExpr
var name value deps = ZVar name deps $ Var value

-- varApp :: Key -> String -> Deps -> ZExpr
-- varApp name value = var name (printf "$%s:%s" name value)


export :: Key -> ZExpr
export name = ZExport (printf "export %s" name) [name]


alias, aliasS, aliasG :: Key -> String -> Deps -> ZExpr
alias  = alias' AliasNormal
aliasS = alias' AliasSuffix
aliasG = alias' AliasGlobal

alias' :: AliasType -> Key -> String -> Deps -> ZExpr
alias' atype name value deps =
    ZAlias name deps $ Alias value atype


path :: [String] -> ZExpr
path ps = var "PATH" (printf "$PATH:%s" $ intercalate ":" ps) []


isUnique :: [ZExpr] -> Bool
isUnique
    = all (== 1)
    . map length
    . groupBy ((==) `on` zKey)
    . sort


fromKey :: String -> [ZExpr] -> ZExpr
fromKey k = fromJust . find ((== k) . zKey)


-- isValid :: [ZExpr] -> Bool
-- isValid conf = all ($ conf) [depsExist, validOrder]


depsExist :: [ZExpr] -> Bool
depsExist conf = flip all conf $
    all (`elem` map zKey conf) . zDeps


-- validOrder :: [ZExpr] -> Bool
-- validOrder conf = flip all conf $
--     \e -> not . any ((zKey e `elem`) . zDeps) $
--                     elemsBefore e

--     where
--     elemsBefore e = takeWhile (/= e) conf


-- missingDeps :: [ZExpr] -> [(Key, Deps)]
-- missingDeps conf
--     = filter (not . null . snd)
--     . map (zKey &&&
--            filter (not . (`elem` map zKey conf)) .
--                   zDeps)
--     $ conf


-- invalidOrder :: [ZExpr] -> (ZExpr, [ZExpr])
-- invalidOrder conf
--     = (,) culprit
--     . filter ((zKey culprit `elem`) . zDeps) $
--              elemsBefore culprit

--     where
--     culprit = head $ filter (\e -> any ((zKey e `elem`) . zDeps) $ elemsBefore e) conf
--     elemsBefore e = takeWhile (/= e) conf


instance Show ZExpr where
    show expr = case expr of
        ZVar  { zKey  = vname
              , zDeps = vdeps
              , zVar  = Var vvalue
              } ->
            printf "%s=%s%s\n"
                vname
                (quote vvalue)
                $ comment vdeps


        ZExport { zKey       = vname
                , zDeps      = vdeps
                } ->
            printf "%s%s\n"
                vname
                $ comment vdeps


        ZAlias { zKey   = aname
               , zDeps  = adeps
               , zAlias = Alias avalue atype
               } ->
            printf "alias %s %s=%s%s\n"
                (show atype)
                aname
                (quote avalue)
                $ comment adeps

        where
        comment []   = ""
        comment deps = printf "\t\t# %s"
                     . intercalate ", "
                     $ sort deps

        quote :: String -> String
        quote = printf "'%s'"


instance Ord ZExpr where
    compare = comparing zKey


instance Eq ZExpr where
    (==) = (==) `on` zKey


instance Show AliasType where
    show atype = case atype of
        AliasNormal -> "  "
        AliasSuffix -> "-s"
        AliasGlobal -> "-g"


data ZExpr
    = ZVar
          { zKey    :: Key
          , zDeps   :: Deps
          , zVar    :: Var
          }
    | ZExport
          { zKey    :: Key
          , zDeps   :: Deps
          }
    | ZAlias
          { zKey    :: Key
          , zDeps   :: Deps
          , zAlias  :: Alias
          }


type Key  = String
type Deps = [Key]


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
