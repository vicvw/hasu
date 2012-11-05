module Main where

import Data.List
import Data.Maybe



type Stone       = (Integer, Integer)
data Color       = Black | White | None
                   deriving (Show, Eq)
type Board       = [Territory]
type Territories = [Territory]
data Territory   = Territory  { getStones     :: [Stone] 
                              , getColor      :: Color }
                   deriving (Eq)
data Captured    = Captured   { capturedBlack :: Integer
                              , capturedWhite :: Integer }
data Game        = Game       { gameBoard     :: Maybe Board
                              , gameCaptured  :: Captured }
data StoneCoord  = StoneCoord Integer Integer Color



gameSize = 19
black    = "░░"
white    = "██"
none     = "┼─"



main = putStr
     . show
     -- . hasLiberty (Territory [(1,6)] Black)
     . putStone (1,6) White
     . putStone (4,6) White
     $ game
  where
    game   = Game (return [black1, black2, white1, white2]) (Captured 0 0)
    black1 = Territory [(1,5),(2,5),(3,5),(3,4)] Black
    black2 = Territory [(1,7),(2,7),(3,7)] Black
    black3 = mergeTerritories black1 [black2]
    white1 = Territory [(2,6),(3,6)] White
    white2 = Territory [(4,7)] White



territoryAt :: Stone -> Game -> Maybe Territory
territoryAt stone (Game board _) = board >>= listToMaybe . filter (elem stone . getStones)



neighbors :: Territory -> Game -> Territories
neighbors territory@(Territory stones color) game@(Game board _) =
    delete territory . nub . concatMap neighborsOfStone $ stones
  where
    neighborsOfStone (x, y) = concatMap (maybeToList . flip territoryAt game)
                                        [(x, y + 1), (x + 1, y), (x, y - 1), (x - 1, y)]



mergeTerritories :: Territory -> Territories -> Territory
mergeTerritories territory@(Territory stones color) territories =
    Territory (concat $ stones : map getStones sameColor) color
  where
    sameColor = filter ((==) color . getColor) territories



changeBoard :: (Board -> Maybe Board) -> Game -> Game
changeBoard f (Game board captured) = Game (f =<< board) captured



changeCaptured :: (Captured -> Captured) -> Game -> Game
changeCaptured f (Game board captured) = Game board (f captured)



incrementCaptured :: Color -> Integer -> Captured -> Captured
incrementCaptured color x (Captured black white) = case color of
    Black -> Captured (black + x) white
    White -> Captured black (white + x)



putStone :: Stone -> Color -> Game -> Game
putStone stone color game@(Game board _) = changeBoard f game
  where
    sameColor        = filter ((==) color . getColor)
    territory        = Territory [stone] color
    neighborsOfStone = sameColor . neighbors territory $ game
    newTerritory     = mergeTerritories territory neighborsOfStone
    f board          = case territoryAt stone game of
                           Just _  -> Nothing
                           Nothing -> return
                                    . (newTerritory :)
                                    . (\\ neighborsOfStone)
                                    $ board



removeTerritoryAt :: Stone -> Game -> Game
removeTerritoryAt stone game@(Game board _) = changeBoard f game
  where
    f board = case territoryAt stone game of
        Just t  -> return . delete t $ board
        Nothing -> Nothing



hasLiberty :: Territory -> Game -> Bool
hasLiberty (Territory stones _) game@(Game board _) = any (not . isSurrounded) $ stones
  where
    isSurrounded stone@(x,y) = all isJust . map (flip territoryAt game) $ possibleNeighbors stone
    possibleNeighbors (x,y) = filter (\(x,y) -> elem x [1..gameSize] && elem y [1..gameSize]) [(x, y + 1), (x + 1, y), (x, y - 1), (x - 1, y)]



instance Show StoneCoord where
    show (StoneCoord x y color) = case color of
        Black -> black
        White -> white
        None  -> none



instance Eq StoneCoord where
    (StoneCoord x1 y1 _) == (StoneCoord x2 y2 color2) = (x1,y1) == (x2,y2)



instance Ord StoneCoord where
    compare (StoneCoord x1 y1 _) (StoneCoord x2 y2 _) = compare (-y1,x1) (-y2,x2)



instance Show Territory where
    show (Territory stones color) = show color ++ ": " ++ (show . sort $ stones)



instance Show Game where
    show (Game board _) = case board of
        Just board' -> unlines
                     . map (concatMap show)
                     . chunk gameSize
                     . sort
                     . union convertedBoard
                     $ [StoneCoord x y None | x <- [1..gameSize], y <- [1..gameSize]]
          where
            convertedBoard = concat . map convertTerritory $ board'
            convertTerritory (Territory stones color) = map (\(x,y) -> StoneCoord x y color) stones
        Nothing     -> "You die."

      where
        chunk :: Integer -> [a] -> [[a]]
        chunk _ [] = []
        chunk n xs = y1 : chunk n y2
          where
            (y1, y2) = splitAt (fromInteger n) xs
