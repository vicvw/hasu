module Omo
    ( different
    , 無, 全, 風, 空
    , Diff
    , DiffT
    ) where


import Control.Applicative  ((<$>))
import Data.Maybe           (fromMaybe)


different :: a -> (DiffT a) -> IO a
different failed diff = (<$> hostname)
    $ \host -> fromMaybe failed
    . ($ diff empty)
    $ case host of
    "kaze" -> kaze
    "sora" -> sora

    where
    hostname = takeWhile (/= '\n')
           <$> readFile "/etc/hostname"

    empty = 無 undefined


無 :: DiffT a
無 = const $ Diff Nothing Nothing

全, 風, 空 :: a -> DiffT a
全 x      = foldl1 (.) . map ($ x) $ [風, 空]
風 x diff = diff { kaze = Just x }
空 x diff = diff { sora = Just x }


data Diff a = Diff
    { kaze, sora :: Maybe a
    } deriving (Show)


type DiffT a = Diff a -> Diff a
