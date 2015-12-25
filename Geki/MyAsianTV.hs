module MyAsianTV where


import Common


import Data.List          (isInfixOf)

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.HandsomeSoup  (css)
import Text.XML.HXT.Core  (getText, hasText, hread, removeAllWhiteSpace, runLA, when, (>>>), (/>))


url :: String
url = "http://myasiantv.se"


episodes :: Parser [Episode]
episodes = do
    name <- manyTill anyChar . try $ string " Ep "
    ep   <- many1 digit
    sub  <- optionMaybe . try $ string " (" *> string "Sub: " *> many1 digit
    return $ [Episode MyAsianTV name (read ep) (read <$> sub)]


links :: String -> [String]
links html
    = ($ html)
    . runLA
    $ hread
    >>> css "div.lastest"
    >>> css "a" `when` (css "h3" /> hasText ("SUBBED" `isInfixOf`))
    >>> removeAllWhiteSpace
     /> getText
