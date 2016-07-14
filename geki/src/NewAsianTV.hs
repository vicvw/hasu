module NewAsianTV (spec) where


import Data.List          (isPrefixOf)
import Text.HandsomeSoup  (css, (!))
import Text.XML.HXT.Core  (getText, hasAttrValue, hread, removeAllWhiteSpace, runLA, (>>>), (/>), (<+>))
import Text.Megaparsec


import Nara

import Common


spec :: Spec
spec = Spec
    { url = "http://newasiantv.to"

    , parser = do
        name <- manyTill anyChar $ string "\xa0 ["
        hd   <- lookAhead $ some anyChar
        if "HD" `isPrefixOf` hd
        then
            return [Episode NewAsianTV name 0 False]
        else do
            string "Ep "
            ep  <- some digitChar
            space
            sub <- string "RAW" <|> string "Engsub"
            return [Episode NewAsianTV name (read ep) (sub /= "RAW")]

    , links = \html
       -> uncurry (zipWith (++))
        . (\l -> splitAt ((length l + 1) `div` 2) l)
        . ($ html)
        . runLA
        $ hread
        >>> css "#hotdrama div"
        >>> hasAttrValue "rel" (== "updating")
        >>> removeAllWhiteSpace
        >>> css "a" ! "title" <+> (css "li" /> getText)
    }
