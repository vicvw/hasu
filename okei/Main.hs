{-# LANGUAGE FlexibleInstances #-}

module Main where

import Data.List
import Data.Ord
import Data.Maybe
import Text.Printf
import System.IO
import System.Cmd
import Control.Arrow
-- import Database.HDBC
-- import Database.HDBC.Sqlite3



type Word    = String
type Count   = Integer
data Score a = Score { getThing :: a
                     , getCount :: Count }
type Index   = Integer
type IndexedScore a = (Index, Score a)

instance Show (Score Word) where
    show (Score word count) = printf "%i\t%s" count word

instance Eq (Score Word) where
    (Score _ c1) == (Score _ c2) = c1 == c2

instance Ord (Score Word) where
    compare (Score _ c1) (Score _ c2) = compare c2 c1



main = countLoop []
  where
    -- testCounts = incrementCounts ["okei","blub","okei","okei","etcetera","okei","etcetera"] []

    countLoop counts = do
        System.Cmd.system "clear"
        printCounts counts
        word <- getInput counts
        countLoop $ incrementCount word counts



incrementCount = (:)
incrementCounts = foldr (:)



printCounts :: [Word] -> IO ()
printCounts = putStr . unlines . format . toIndexedScore
  where
    -- format = map (\(i, wc) -> printf "(%i)  " (i :: Count) ++ show wc)
    format = map (uncurry (++) . (first (printf "(%i)\t" :: Count -> String) >>> second show))



toIndexedScore :: [Word] -> [IndexedScore Word]
toIndexedScore = zip [1..] . sort . mapCount . group . sort
  where
    mapCount = map (\list@(word:_) -> Score word (length' list))



getInput :: [Word] -> IO Word
getInput list = do
    let score = toIndexedScore list

    putStr "> "
    hFlush stdout
    line <- getLine

    let count = readMaybe line :: Maybe Count
    
    case count of
      Just c  -> if c > 0 && c <= length' score
                   then return $ lookup' c score
                   else getInput list
      Nothing -> if line /= "" && head line /= ' '
                   then return line
                   else getInput list

  where
    lookup' index = getThing . fromJust . lookup index
    readMaybe string = case reads string of
                         [(x, "")] -> Just x
                         _         -> Nothing



length' :: [a] -> Integer
length' = fromIntegral . length
