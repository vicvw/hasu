module TicTacFoot where

import Data.List      (intercalate, intersperse, sortBy, unionBy)
import Data.Ord       (comparing)
import Data.Function  (on)
import Control.Arrow  ((&&&))


type Board  = [Play]

data Play   = Play
    { pPosition :: Position
    , pStone    :: Stone }
    deriving (Eq)

data Position
    = TL | TC | TR
    | CL | CC | CR
    | BL | BC | BR
    deriving (Eq, Ord)

data Stone = X | O | None
    deriving (Eq)


main = do
    putStrLn . showBoard $ test
    print . isWon $ test
    where
        test = map (uncurry Play)
                   [ (TL, X),(TC, X)
                   ,         (CC, X),(CR, O)
                   ,         (BC, O),(BR, O) ]


isWon :: Board -> Bool
isWon board = any (all (flip elem board)) winStates


winStates :: [Board]
winStates = concatMap (\xs -> uncurry (++)
                            . (($ repeat X) &&& ($ repeat O))
                            $ return . zipWith Play xs)
                      lines
    where
        lines = [ [TL, TC, TR]
                , [CL, CC, CR]
                , [BL, BC, BR]
                , [TL, CL, BL]
                , [TC, CC, BC]
                , [TR, CR, BR]
                , [TL, CC, BR]
                , [TR, CC, BL] ]


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


instance Show Stone where
    show stone = case stone of
        X     -> " メ "
        O     -> " ロ "
        None  -> "    "
