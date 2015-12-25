module IKShow where


import Common


import Text.Parsec
import Text.Parsec.String (Parser)
import Text.HandsomeSoup  (css)
import Text.XML.HXT.Core  (getText, hread, removeAllWhiteSpace, runLA, when, (>>>), (/>))


url :: String
url = "http://ikshow.net/shows/k-pop-star-s5/"


episodes :: Parser [Episode]
episodes = do
    name <- manyTill anyChar . try $ string " Episode "
    ep   <- many1 digit
    return [Episode IKShow name (read ep) Nothing]


links :: String -> [String]
links html
    = ($ html)
    . runLA
    $ hread
    >>> css "#list-episodes h2"
    >>> css "a" `when` css ".label-sub"
    >>> removeAllWhiteSpace
     /> getText
