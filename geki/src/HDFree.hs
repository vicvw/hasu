module HDFree (spec) where


import Control.Arrow      (second, (&&&))

import Text.HandsomeSoup  (css, (!))
import Text.XML.HXT.Core  (getText, hread, runLA, (>>>), (/>))
import Text.Megaparsec

import Nara (fi)


import Common


spec :: Spec
spec = Spec
    { url = "http://hdfree.se"

    , parser = do
        name  <- manyTill anyChar $ string " Ep"
        ep    <- read <$> some digitChar
        manyTill anyChar $ char '('
        sub   <- read <$> some digitChar

        return $
            [ Episode HDFree name sub True ]
            ++
            fi (ep > sub)
                [ Episode HDFree name ep  False ]
                []

    , links = \html
       -> map (uncurry (++))
        . ($ html)
        . runLA
        $ hread
      >>> css ".left-home .line-title h4"
      >>> css "a" /> getText
      &&& css "b" /> getText
    }
