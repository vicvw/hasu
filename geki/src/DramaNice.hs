module DramaNice (spec) where


import Data.List          (nub)

import Text.HandsomeSoup  (css)
import Text.XML.HXT.Core  (getText, hread, removeAllWhiteSpace, runLA, (>>>), (/>))--, (<+>))
import Text.Megaparsec


import Common


spec :: Spec
spec = Spec
    { url = "http://dramanice.eu"

    , parser = do
        name <- manyTill anyChar $ string " Episode "
        ep   <- some digitChar
        return [Episode DramaNice name (read ep) True]

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
