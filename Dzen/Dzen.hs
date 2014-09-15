module Dzen.Dzen
    ( dzen
    , runDzen
    , (<|>)
    , module Dzen.DzenInstances
    ) where


import Dzen.DzenInstances

import Control.Monad  (void)
import Data.List      (nub)
import System.Process (readProcess, system)
import Text.Printf    (printf)


dzen :: [DzenOption] -> String
-- dzen :: [DzenOption] -> [String]
dzen = printf "%s %s" dzenCmd . unwords . map show . nub
-- dzen = concatMap ((\(x, y) -> [x, tail y]) . break (== ' ')) . map show . nub
    where
    dzenCmd = "dzen2"


runDzen :: String -> IO ()
-- runDzen :: [String] -> IO ()
-- runDzen options = void $ readProcess dzenCmd options ""
--     where
--     dzenCmd = "dzen2"
runDzen = void . system


infixr 9 <|>
(<|>) :: String -> String -> String
(<|>) = printf "%s | %s"
