module Main (main) where


import Data.List
import Data.Maybe
import System.Cmd


gameSize :: Integer
black, none, white :: String

gameSize = 19
black    = "░░"
white    = "██"
none     = "┼─"


-- main = putStr
--      . show
--      -- . hasLiberty (Territory [(1,6)] Black)
--      . putStone (1,6) White
--      . putStone (4,6) White
--      $ game
  -- where
  --   game   = Game (return [black1, black2, white1, white2]) (Captured 0 0)
  --   black1 = Territory [(1,5),(2,5),(3,5),(3,4)] Black
  --   black2 = Territory [(1,7),(2,7),(3,7)] Black
  --   black3 = mergeTerritories black1 [black2]
  --   white1 = Territory [(2,6),(3,6)] White
  --   white2 = Territory [(4,7)] White


main :: IO ()
main = mainLoop (cycle [Black, White])
              $ Game (return []) (Captured 0 0)

    where
    mainLoop colors game = do
        system "clear"
        putStrLn . show $ game

        coord <- inputCoord

        let colors' = tail colors
            game'@(Game board _) = putStone coord (head colors) game

        maybe
            ( mainLoop colors game )
            ( const $ mainLoop colors' game' )
            board


territoryAt :: Stone -> Game -> Maybe Territory
territoryAt stone (Game board _) =
    board >>= listToMaybe . filter (elem stone . getStones)


neighbors :: Territory -> Game -> Territories
neighbors territory@(Territory stones _) game =
    delete territory . nub . concatMap neighborsOfStone $ stones

    where
    neighborsOfStone (x, y) = concatMap
        (maybeToList . flip territoryAt game)
        [(x, y + 1), (x + 1, y), (x, y - 1), (x - 1, y)]


mergeTerritories :: Territory -> Territories -> Territory
mergeTerritories (Territory stones color) territories =
    Territory (concat $ stones : map getStones sameColor) color

    where
    sameColor = filter
        ((== color) . getColor)
        territories


changeBoard :: (Board -> Maybe Board) -> Game -> Game
changeBoard f (Game board captured) = Game (f =<< board) captured


-- changeCaptured :: (Captured -> Captured) -> Game -> Game
-- changeCaptured f (Game board captured) = Game board (f captured)


-- incrementCaptured :: Color -> Integer -> Captured -> Captured
-- incrementCaptured color x (Captured black white) = case color of
--     Black -> Captured (black + x) white
--     White -> Captured black (white + x)


putStone :: Stone -> Color -> Game -> Game
putStone stone color game = changeBoard f game
    where
    -- TODO: bullshit
    f board = if insideBoard
              then
                  if allowedToPut stone color game
                  then return . (newTerritory :) . (\\ neighborsOf) $ board
                  else Nothing
              else Nothing

    sameColor    = filter ((== color) . getColor)
    territory    = Territory [stone] color
    neighborsOf  = sameColor . neighbors territory $ game
    newTerritory = mergeTerritories territory neighborsOf
    insideBoard  = let (x,y) = stone
                   in  all (flip elem [1..gameSize]) [x, y]


allowedToPut :: Stone -> Color -> Game -> Bool
allowedToPut stone color game
    = null deadNeighbors
   || any ((== enemy color) . getColor) deadNeighbors

    where
    fakeGame      = putStone stone None game
    deadNeighbors = filter
        (not . flip hasLiberty fakeGame)
        (neighbors (Territory [stone] None) fakeGame)

    enemy Black   = White
    enemy White   = Black


-- removeTerritoryAt :: Stone -> Game -> Game
-- removeTerritoryAt stone game@(Game board _) = changeBoard f game
--   where
--     f board' = case territoryAt stone game of
--         Just t  -> return . delete t $ board'
--         Nothing -> Nothing


hasLiberty :: Territory -> Game -> Bool
hasLiberty (Territory stones _) game =
    any (not . isSurrounded) $ stones

    where
    isSurrounded stone
        = all isJust
        . map (`territoryAt` game)
        . possibleNeighbors
        $ stone

    possibleNeighbors (x, y) = filter
        (\(x', y') -> all (`elem` [1..gameSize]) [x', y'])
        [ (x,   y+1)
        , (x+1, y  )
        , (x,   y-1)
        , (x-1, y  ) ]


inputCoord :: IO Stone
inputCoord = do
    line <- getLine
    let [(x, rest)] = reads line
        [(y, _)]    = reads rest
    return (x, y)


instance Show StoneCoord where
    show (StoneCoord _ _ color) = case color of
        Black -> black
        White -> white
        None  -> none


instance Eq StoneCoord where
    (StoneCoord x1 y1 _) == (StoneCoord x2 y2 _) =
        (x1,y1) == (x2,y2)


instance Ord StoneCoord where
    (StoneCoord x1 y1 _) `compare` (StoneCoord x2 y2 _) =
        (-y1, x1) `compare` (-y2, x2)


instance Show Territory where
    show (Territory stones color) = concat
        [ show color, ": ", show . sort $ stones ]


instance Show Game where
    show (Game board _) = flip (maybe "You die.") board $
        \board' -> unlines
                 . map (concatMap show)
                 . chunk gameSize
                 . sort
                 . union (convertedBoard board')
                 $ [ StoneCoord x y None
                   | x <- [1..gameSize]
                   , y <- [1..gameSize] ]

        where
        convertedBoard board' =
            concat . map convertTerritory $ board'
        convertTerritory (Territory stones color) =
            (\(x,y) -> StoneCoord x y color) `map` stones

        chunk _ [] = []
        chunk n xs = y1 : chunk n y2
            where
            (y1, y2) = splitAt (fromInteger n) xs


type Stone       = (Integer, Integer)
type Board       = [Territory]
type Territories = [Territory]

data Color
    = Black
    | White
    | None
    deriving (Show, Eq)

data Territory = Territory
    { getStones :: [Stone]
    , getColor  :: Color }
    deriving (Eq)

data Captured = Captured
    Integer
    Integer

data Game = Game
    (Maybe Board)
    Captured

data StoneCoord = StoneCoord
    Integer
    Integer
    Color
