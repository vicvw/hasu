module Omo
    ( different
    , onHost
    , 風, 空
    , 全, 無
    , Diff
    , DiffT
    ) where


import Control.Applicative  ((<$>))
import Data.Maybe           (fromMaybe)


different :: a -> DiffT a -> IO a
different def diff = (<$> hostname)
    $ fromMaybe def
    . (`lookup` diff [])

    where
    hostname = takeWhile (/= '\n')
           <$> readFile "/etc/hostname"


風, 空 :: a -> DiffT a
風 = onHost "kaze"
空 = onHost "sora"

全 :: a -> DiffT a
全 x = foldl1 (.) . map ($ x) $ [風, 空]

無 :: DiffT a
無 = const []

onHost :: String -> a -> DiffT a
onHost host x = ((host, x) :)


type Diff  a = [(String, a)]
type DiffT a = Diff a -> Diff a
