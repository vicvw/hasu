module MyAsianTV (spec) where


import Data.List          (isInfixOf)

import Text.HandsomeSoup  (css)
import Text.XML.HXT.Core  (getText, hasText, hread, removeAllWhiteSpace, runLA, when, (>>>), (/>))
import Text.Megaparsec


import Common


spec :: Spec
spec = Spec
    { url = "http://myasiantv.se"

    , parser = do
        name <- manyTill anyChar $ string " Ep "
        ep   <- some digitChar
        sub  <- optional $ string " (" *> string "Sub: " *> some digitChar
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
