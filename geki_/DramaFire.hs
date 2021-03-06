module DramaFire where


import Common


import Nara


import Data.List          (isInfixOf)
import Data.Maybe         (isNothing)

import Text.HTML.TagSoup  ((~==), (~/=), fromTagText, isTagText, parseTags, partitions)
import Text.Megaparsec


spec :: Spec
spec = Spec
    { url = "http://dramafire.com"

    , parser = do
        name <- manyTill anyChar . try $ string " episode"
        optional $ char 's'
        space
        ep   <- some digitChar
        more <- optional $ char '-'

        fi (isNothing more)
            (return [Episode DramaFire name (read ep) Nothing])
            $ do
                ep' <- some digitChar <?> name
                return $ (<$> [read ep .. read ep']) $ \n ->
                    Episode DramaFire name n Nothing

    , links
        = filter ("episode" `isInfixOf`)
        . map fromTagText
        . filter isTagText
        . takeWhile (~/= "<div class='recent-from recent-last et-recent-top'>")
        . concat
        . partitions (~== "<div class='recent-from et-recent-top'>")
        . parseTags
    }
