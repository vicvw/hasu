module DramaLove (spec) where


import Control.Arrow      (second, (&&&))

import Data.List          (isInfixOf, isPrefixOf)

import Text.HandsomeSoup  (css, (!))
import Text.XML.HXT.Core  (getText, hread, runLA, (>>>), (/>), (<+>))
import Text.Megaparsec


import Nara

import Common


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
       -> map (uncurry (++))
        . filter (("Episode" `isPrefixOf`) . snd)
        . ($ html)
        . runLA
        $ hread
        >>> css ".recent div"
        >>> (css ".title"         /> getText)
        &&& (css ".episode-type"  /> getText)
    }
