module DramaCool (spec) where


import Control.Arrow      (second, (&&&))

import Text.HandsomeSoup  (css, (!))
import Text.XML.HXT.Core  (getText, hread, runLA, (>>>), (/>))
import Text.Megaparsec
import Debug.Trace


import Common


spec :: Spec
spec = Spec
    { url = "http://www.ondramacool.com"

    , parser = do
        name  <- manyTill anyChar $ string "EP "
        ep    <- some digitChar
        -- space
        -- sub   <- string "raw" <|> string "sub"

        -- return [Episode DramaCool name (read ep) (sub == "sub")]
        return [Episode DramaCool name (read ep) True]

    , links = \html
       -- -> traceShowId
        -- . map (uncurry (++) . second (dropWhile (/= ' ')))
       -> map (uncurry (++))
        . ($ html)
        . runLA
        $ hread
        >>> css ".left-tab-1 li a"
        >>> (css ".title"   /> getText)
        &&& (css ".ep.SUB"  /> getText)
    }
