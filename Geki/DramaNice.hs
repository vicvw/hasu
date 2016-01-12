module DramaNice where


import Common


import Text.Parsec
import Text.Parsec.String (Parser)
import Text.HandsomeSoup  (css)
import Text.XML.HXT.Core  (getText, hread, removeAllWhiteSpace, runLA, when, (>>>), (/>))


url :: String
url = "http://www.dramanice.to/drama/survival-audition-k-pop-star-s5-detail"


episodes :: Parser [Episode]
episodes = do
    string "Episode "
    ep   <- many1 digit
    return [Episode DramaNice "K-Pop Star (S5)" (read ep) Nothing]


links :: String -> [String]
links html
    = ($ html)
    . runLA
    $ hread
    >>> css ".list-episodes div a"
    >>> css "span" `when` css ".SUB"
    >>> removeAllWhiteSpace
     /> getText
