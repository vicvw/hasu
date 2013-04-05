module TicTacFoot where


import Data.List      (intercalate, intersperse, sortBy, unionBy)
import Data.Ord       (comparing)
import Data.Function  (on)
import Control.Monad.State
-- import Control.Arrow  ((&&&))
-- import System.IO
-- import System.Cmd     (system)


main :: IO ()
main = do
    let (board, (Game _ gs)) = runState test empty
    print gs
    printBoard board

    where
    empty = Game [] OK
    test = do
        move CC X
        move CC O


move :: Position -> Stone -> GameS
move pos stone = state $ \(Game board gs) ->
    if newPlay `elem` board
    then
        (board, Game board (Illegal stone))
    else
        let newBoard = play newPlay board
        in  (newBoard, Game newBoard OK)

    where
    newPlay = Play pos stone


play :: Play -> Board -> Board
play = (:)


printBoard :: Board -> IO ()
printBoard = putStrLn . showBoard


showBoard :: Board -> String
showBoard
    = concat
    . intercalate ["\n────┼────┼────\n"]
    . map (intersperse "│")
    . chunk 3
    . map (show . pStone)
    . sortBy (comparing pPosition)
    . flip (unionBy ((==) `on` pPosition)) emptyBoard
    where
        emptyBoard = zipWith Play
                             [TL, TC, TR , CL, CC, CR , BL, BC, BR]
                             (repeat None)

        chunk n = foldr ((:) . take n) []
                . takeWhile (not . null)
                . iterate (drop n)


type GameS = State Game Board

data Game = Game
    { gameBoard :: Board
    , gameState :: GameState }
    deriving (Show)

data GameState
    = OK
    | Illegal Stone
    | Over    OverState
    deriving (Show)

data OverState
    = Tied
    | WonBy Stone
    deriving (Show)

type Board = [Play]

data Play = Play
    { pPosition :: Position
    , pStone    :: Stone }
    -- deriving (Eq)
    deriving (Show)

data Position
    = TL | TC | TR
    | CL | CC | CR
    | BL | BC | BR
    deriving (Read, Show , Eq, Ord)

data Stone = X | O | None
    deriving (Eq)


instance Show Stone where
    show stone = case stone of
        X     -> " メ "
        O     -> " ロ "
        None  -> "    "


instance Eq Play where
    (Play p1 s1) == (Play p2 s2) =
        p1 == p2 && all (/= None) [s1, s2]
