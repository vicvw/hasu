module Hosuto
    ( run
    , launcherFromList
    , getHostname
    , lHostname
    , chunk )
    where


import Control.Concurrent   (threadDelay)
import Control.Monad        (void)

import System.Posix.Process (forkProcess)
import System.Process       (system, readProcess)


run :: Launcher -> IO ()
run (Launcher _ delay cmd) = do
    threadDelay . truncate $ delay * 10^6
    void . forkProcess . void $ system cmd


launcherFromList :: [String] -> Launcher
launcherFromList [host, delay, cmd] =
    Launcher (toHostname host) (read delay) cmd
launcherFromList _ = error "bad list"


getHostname :: IO Hostname
getHostname = toHostname `fmap` hostname

    where
    hostname = takeWhile (/= '\n')
        `fmap` readProcess "hostname" [] ""


toHostname :: String -> Hostname
toHostname host = case host of
    "kaze"   ->  Kaze
    "heaven" ->  Heaven
    _        ->  Illegal


chunk :: Int -> [a] -> [[a]]
chunk n = foldr ((:) . take n) []
        . takeWhile (not . null)
        . iterate (drop n)


data Hostname
    = Kaze
    | Heaven
    | Illegal
    deriving (Show, Eq)


data Launcher = Launcher
    { lHostname  :: Hostname
    , lDelay     :: Double
    , lCommand   :: String }
    deriving Show
