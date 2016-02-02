module IKShow (spec) where


import Text.Parsec
import Text.HandsomeSoup  (css)
import Text.XML.HXT.Core  (getText, hread, removeAllWhiteSpace, runLA, when, (>>>), (/>))


import Common


spec :: Spec
spec = Spec
    { url = "http://ikshow.net/shows/k-pop-star-s5/"

    , parser = do
        name <- manyTill anyChar . try $ string " Episode "
        ep   <- many1 digit
        return [Episode IKShow name (read ep) Nothing]

    , links = \html
       -> ($ html)
        . runLA
        $ hread
        >>> css "#list-episodes h2"
        >>> css "a" `when` css ".label-sub"
        >>> removeAllWhiteSpace
         /> getText
    }
