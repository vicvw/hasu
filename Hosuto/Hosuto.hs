module Hosuto
    ( run
    , launcherFromList
    , getHostname
    , lHostname
    , chunk )
    where


import System.Process     (system, readProcess)
import Control.Concurrent (threadDelay)


data Hostname
    = Paradise
    | Heaven
    | Illegal
    deriving (Show, Eq)


data Launcher = Launcher
    { lHostname  :: Hostname
    , lDelay     :: Double
    , lCommand   :: String }
    deriving Show


run :: Launcher -> IO ()
run (Launcher _ delay cmd) = do
    threadDelay . truncate $ delay * 10^6
    system cmd
    return ()


launcherFromList :: [String] -> Launcher
launcherFromList [host, delay, cmd] =
    Launcher (toHostname host) (read delay) cmd
launcherFromList _ = error "bad list"


getHostname :: IO Hostname
getHostname = do
    host <- hostname'
    return $ toHostname host

    where
    hostname' = takeWhile (/= '\n')
         `fmap` readProcess "hostname" [] ""


toHostname :: String -> Hostname
toHostname host = case host of
    "paradise"  ->  Paradise
    "heaven"    ->  Heaven
    _           ->  Illegal


chunk n = foldr ((:) . take n) []
        . takeWhile (not . null)
        . iterate (drop n)
