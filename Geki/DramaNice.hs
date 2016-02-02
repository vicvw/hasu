module DramaNice (spec) where


import Data.List          (nub)

import Text.Parsec
import Text.HandsomeSoup  (css)
import Text.XML.HXT.Core  (getText, hread, removeAllWhiteSpace, runLA, (>>>), (/>))--, (<+>))


import Common


spec :: Spec
spec = Spec
    { url = "http://dramanice.to"

    , parser = do
        name <- manyTill anyChar . try $ string " Episode "
        ep   <- many1 digit
        return [Episode DramaNice name (read ep) Nothing]

    , links = \html
       -> nub
        . ($ html)
        . runLA
        $ hread
        >>> css "#recentadd" -- <+> css "#recentpopular"
        >>> css ".sub .info-name a span"
        >>> removeAllWhiteSpace
         /> getText
    }
