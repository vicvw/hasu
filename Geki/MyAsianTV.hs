module MyAsianTV (spec) where


import Data.List          (isInfixOf)

import Text.Parsec
import Text.HandsomeSoup  (css)
import Text.XML.HXT.Core  (getText, hasText, hread, removeAllWhiteSpace, runLA, when, (>>>), (/>))


import Common


spec :: Spec
spec = Spec
    { url = "http://myasiantv.se"

    , parser = do
        name <- manyTill anyChar . try $ string " Ep "
        ep   <- many1 digit
        sub  <- optionMaybe . try $ string " (" *> string "Sub: " *> many1 digit
        return [Episode MyAsianTV name (read ep) (read <$> sub)]

    , links = \html
       -> ($ html)
        . runLA
        $ hread
        >>> css "div.lastest"
        >>> css "a" `when` (css "h3" /> hasText ("SUBBED" `isInfixOf`))
        >>> removeAllWhiteSpace
         /> getText
    }
