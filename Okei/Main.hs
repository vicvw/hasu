{-# LANGUAGE FlexibleInstances #-}


module Main (main) where


import Data.List      (group, sort)
import Data.Maybe     (fromJust)
import Text.Printf    (printf)
import System.IO      (hFlush, stdout)
import System.Cmd     (system)
import Control.Arrow  ((***))



main :: IO ()
main = countLoop []
    where
    countLoop counts = do
        system "clear"
        printCounts counts

        word <- getInput counts
        countLoop $ incrementCount word counts

    -- testCounts = incrementCounts ["okei","blub","okei","okei","etcetera","okei","etcetera"] []


incrementCount :: a -> [a] -> [a]
incrementCount = (:)


printCounts :: [Word] -> IO ()
printCounts
    = putStr
    . unlines
    . format
    . toIndexedScore

    where
    format = map $ uncurry (++) . (printf "(%i)\t" *** show)


toIndexedScore :: [Word] -> [IndexedScore Word]
toIndexedScore
    = zip [1..]
    . sort
    . mapCount
    . group
    . sort

    where
    mapCount = map $ \list@(word:_) -> Score word $ length' list


getInput :: [Word] -> IO Word
getInput list = do
    let score = toIndexedScore list

    putStr "> "
    hFlush stdout

    line <- getLine

    maybe
        ( if line /= "" && head line /= ' '
            then return line
            else getInput list )

        ( \c -> if c `elem` [1..length' score]
                then return $ lookup' c score
                else getInput list )

        $ readMaybe line

    where
    lookup' index = getThing . fromJust . lookup index

    readMaybe string = case reads string of
        [(x, "")] -> Just x
        _         -> Nothing


length' :: [a] -> Integer
length' = fromIntegral . length


type Word  = String
type Count = Integer
type Index = Integer
type IndexedScore a = (Index, Score a)

data Score a = Score
    { getThing :: a
    , getCount :: Count }


instance Show (Score Word) where
    show (Score word count) = printf "%i\t%s" count word


instance Eq (Score Word) where
    (Score _ c1) == (Score _ c2) = c1 == c2


instance Ord (Score Word) where
    compare (Score _ c1) (Score _ c2) = compare c2 c1
