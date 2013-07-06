module TicTacFoot (main) where


import Data.List      (intercalate, intersperse, sortBy, unionBy)
import Data.Ord       (comparing)
import Data.Function  (on)
import Control.Arrow  ((&&&))
import System.IO      (hFlush, stdout)
import System.Cmd     (system)


main :: IO ()
main = loop players []
    where
    loop (p:ps) board = do
        system "clear"

        if isWon board
        then putStrLn $ show p ++ " 勝った。"
        else do
            print . isWon $ board
            printBoard board

            pos <- getInput
            print pos

            loop ps $ move pos p board

    players = cycle [X, O]

    -- test = map (uncurry Play)
    --            [ (TL, X),(TC, X)
    --            ,         (CC, X),(CR, O)
    --            ,         (BC, O),(BR, O) ]


move :: Position -> Stone -> Board -> Board
move pos stone = (Play pos stone :)


getInput :: IO Position
getInput = do
    putStr "> "
    hFlush stdout

    input <- getLine
    return . read $ input


isWon :: Board -> Bool
isWon board = any (all (`elem` board)) winStates


winStates :: [Board]
winStates = concatMap
    (\xs -> uncurry (++)
          . (($ repeat X) &&& ($ repeat O))
          $ return . zipWith Play xs)
    wins

    where
    wins =
        [ [TL, TC, TR]
        , [CL, CC, CR]
        , [BL, BC, BR]
        , [TL, CL, BL]
        , [TC, CC, BC]
        , [TR, CR, BR]
        , [TL, CC, BR]
        , [TR, CC, BL] ]


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
        [TL, TC, TR , CL, CC, CR , BL, BC, BR] $
        repeat None

    chunk n = foldr ((:) . take n) []
            . takeWhile (not . null)
            . iterate (drop n)


type Board = [Play]

data Play = Play
    { pPosition :: Position
    , pStone    :: Stone }
    deriving (Eq)

data Position
    = TL | TC | TR
    | CL | CC | CR
    | BL | BC | BR
    deriving (Read, Show, Eq, Ord)

data Stone = X | O | None
    deriving (Eq)


instance Show Stone where
    show stone = case stone of
        X     -> " メ "
        O     -> " ロ "
        None  -> "    "
