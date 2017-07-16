module HDFree (spec) where


import Control.Arrow      (first, second, (&&&))

import Data.List          (isInfixOf)

import Text.HandsomeSoup  (css, (!))
import Text.XML.HXT.Core  (getText, hread, runLA, (>>>), (/>))
import Text.Megaparsec
import Debug.Trace


import Nara (fi)


import Common


spec :: Spec
spec = Spec
    { url = "http://fastdrama.co"

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
       -- -> traceShowId
       -- -> map (uncurry (++))
       -> filter (" Ep" `isInfixOf`)
        . filter (not . ("Completed" `isInfixOf`))
        . map (uncurry (++) . first  (takeWhile (/= '-')))
        . ($ html)
        . runLA
        $ hread
      >>> css ".grid .image"
      >>> (css "a" ! "title")
      &&& (css ".status" /> getText)
    }
