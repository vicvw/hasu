module NewAsianTV (spec) where


import Data.List          (nub)

import Text.HandsomeSoup  (css, (!))
import Text.XML.HXT.Core  (getText, hasAttrValue, hread, removeAllWhiteSpace, runLA, (>>>), (/>), (<+>))
import Text.Megaparsec


import Common


spec :: Spec
spec = Spec
    { url = "http://newasiantv.to"

    , parser = do
        name <- manyTill anyChar $ string "\xa0 [Ep "
        ep   <- some digitChar
        return [Episode NewAsianTV name (read ep) Nothing]

    , links = \html
       -> uncurry (zipWith (++))
        . (\l -> splitAt ((length l + 1) `div` 2) l)
        . ($ html)
        . runLA
        $ hread
        >>> css "#hotdrama div"
        >>> hasAttrValue "rel" (== "updating")
        >>> removeAllWhiteSpace
        >>> css "a" ! "title" <+> (css "li" /> getText)
    }

    where
    chunk n
        = foldr ((:) . take n) []
        . takeWhile (not . null)
        . iterate (drop n)
