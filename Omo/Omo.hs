module Omo
    ( different
    , 全, 風, 空
    , Diff
    , DiffT
    ) where


import Data.Maybe (fromMaybe)


different :: a -> (Diff a -> Diff a) -> IO a
different failed diff = do
    host <- getHostname

    return
        . fromMaybe failed
        . ($ diff empty)
        $ case host of
        "kaze" -> kaze
        "sora" -> sora

    where
    getHostname = takeWhile (/= '\n')
           `fmap` readFile "/etc/hostname"


全, 風, 空 :: a -> DiffT a
全 x      = 風 x . 空 x
風 x diff = diff { kaze = Just x }
空 x diff = diff { sora = Just x }


empty :: Diff a
empty = Diff
    { kaze = Nothing
    , sora = Nothing
    }


data Diff a = Diff
    { kaze :: Maybe a
    , sora :: Maybe a
    } deriving (Show)


type DiffT a = Diff a -> Diff a
