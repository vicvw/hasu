{-# LANGUAGE FlexibleInstances #-}

module MainDB where

import qualified Data.List as L
import Data.Maybe
import Text.Printf
import System.IO
import System.Cmd
import Control.Monad
import Control.Arrow

import Database.HaskellDB
import Database.HaskellDB.Database
import Database.HaskellDB.FieldType
import Database.HaskellDB.HDBC.SQLite3

import qualified Score as S



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



main = okei $ countLoop
  where
    okei = sqliteConnect "okei.db"

    countLoop db = do
        System.Cmd.system "clear"

        printScore db
        word <- getInput db

        if word == "exit"
          then return ()
          else do
               case word of
                 "clear!"    -> clearScore db
                 ('#':toDel) -> deleteWord toDel db
                 ('-':toDec) -> changeCount toDec (subtract 1) db
                 (toInc)     -> changeCount toInc (+ 1) db

               countLoop db



createTables db = createTable db "score"
    [("word",  (StringT, False))
    ,("count", (IntT,    False))]



clearScore db = delete db S.score $ const (constant True)



deleteWord word db = delete db S.score
    (\score -> score!S.word .==. constant word)



changeCount word f db = do
    rows <- query db $ getCount word

    case rows of
      []    -> insertWord word db
      (r:_) -> let count  = r!S.count
                   count' = f count
               in update db S.score
                   (\score -> score!S.word .==. constant word)
                   (\score -> S.count << constant count')

  where
    getCount word = do
        score <- table S.score
        restrict $ score!S.word .==. constant word
        project  $ S.count << score!S.count

    insertWord word db = insert db S.score $
        S.word  << constant word #
        S.count << constant 1



printScore db = do
    rows <- query db $ table S.score
    putStr . format . toIndexedScore $ rows

  where
    format = unlines . map (uncurry (++) . (first (printf "(%i)\t" :: Count -> String) >>> second show))



toIndexedScore = zip [1..] . L.sort . mapScore
  where
    mapScore    = map (\r -> Score (r!S.word) (toCount $ r!S.count)) 
    toCount int = fromIntegral int :: Count



getInput db = do
    rows <- query db $ table S.score

    let score = toIndexedScore rows

    putStr "> "
    hFlush stdout
    line <- getLine

    let count = readMaybe line :: Maybe Count
    
    case count of
      Just c  -> if c > 0 && c <= length' score
                   then return $ lookup' c score
                   else getInput db
      Nothing -> if line /= "" && head line /= ' '
                   then return line
                   else getInput db

  where
    lookup' index = getThing . fromJust . lookup index
    readMaybe string = case reads string of
                         [(x, "")] -> Just x
                         _         -> Nothing
    length' :: [a] -> Integer
    length' = fromIntegral . length
