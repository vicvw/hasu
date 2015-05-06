module Mosi
    ( fi
    , ii
    , if_
    , if'
    ) where


fi :: Bool -> a -> a -> a
fi = flip $ flip . ii


ii :: a -> a -> Bool -> a
ii t f = if' id (const t) (const f)


if_ :: a -> (a -> Bool) -> (a -> b) -> (a -> b) -> b
if_ x test t f = if' test t f x


if' :: (a -> Bool) -> (a -> b) -> (a -> b) -> a -> b
if' test t f x = ($ x) $ if test x then t else f
