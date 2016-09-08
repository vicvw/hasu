module DramaLove (spec) where


import Data.List          (isPrefixOf)
import Text.HandsomeSoup  (css, (!))
import Text.XML.HXT.Core  (getText, hread, runLA, (>>>), (/>), (<+>))
import Text.Megaparsec


import Nara

import Common
import Debug.Trace


spec :: Spec
spec = Spec
    { url = "http://www.dramalove.tv"

    , parser = do
        name  <- manyTill anyChar $ string "Episode "
        ep    <- some digitChar
        string " - "
        sub   <- string "Sub" <|> string "Raw"

        return [Episode DramaLove name (read ep) (sub == "Sub")]

    , links = \html
       -> map concat . chunk 2
        . ($ html)
        . runLA
        $ hread
        >>> css ".recent div"
        >>> (css ".title"         /> getText)
        <+> (css ".episode-type"  /> getText)
    }

    where
    chunk n
        = foldr ((:) . take n) []
        . takeWhile (not . null)
        . iterate (drop n)
