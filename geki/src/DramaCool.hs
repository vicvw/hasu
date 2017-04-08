module DramaCool (spec) where


import Control.Arrow      (second, (&&&))

import Text.HandsomeSoup  (css, (!))
import Text.XML.HXT.Core  (getText, hread, runLA, (>>>), (/>))
import Text.Megaparsec


import Common


spec :: Spec
spec = Spec
    { url = "http://www1.dramacool.to"

    , parser = do
        name  <- manyTill anyChar $ string " Episode "
        ep    <- some digitChar
        space
        sub   <- string "raw" <|> string "sub"

        return [Episode DramaCool name (read ep) (sub == "sub")]

    , links = \html
       -> map (uncurry (++) . second (dropWhile (/= ' ')))
        . ($ html)
        . runLA
        $ hread
        >>> css "#RecentlyAdded .thumbnail"
        >>> (css ".info span" /> getText)
        &&& (css ".bg-ep img" ! "alt")
    }
