module Main (main) where


import Data.List
import Network

import System.Exit
import System.IO
import System.Time

import Control.Arrow
import Control.Exception
import Control.Monad.Reader

import Text.Printf
import Text.Regex.PCRE


chan, nick, server :: String
port :: Integer

server = "irc.freenode.org"
chan   = "#troia"
nick   = "helena-of-troy"
port   = 6667


main :: IO ()
main = bracket connect disconnect loop
    where
    disconnect = hClose . socket
    loop st    = runReaderT run st `catch` (const $ return () :: SomeException -> IO ())


connect :: IO Bot
connect = notify $ do
    t <- getClockTime
    h <- connectTo server . PortNumber $ fromIntegral port

    hSetBuffering h NoBuffering

    return $ Bot h t

    where
    notify = bracket_
        (printf "Connecting to %s ... " server >> hFlush stdout)
        (putStrLn "done.")


run :: Net ()
run = do
    write "NICK" nick
    write "USER" $ nick ++ " 0 * :tutorial bot"
    write "JOIN" chan

    asks socket >>= listen


listen :: Handle -> Net ()
listen h = forever $ do
    s <- init `fmap` io (hGetLine h)
    io $ putStrLn s

    if ping s
    then pong s
    else eval s

    where
    ping = ("PING :" `isPrefixOf`)
    pong = write "PONG" . (':' :) . drop 6


eval :: String -> Net ()
eval x
    | x' =~ "^;quit"   = write "QUIT" ":Exiting" >> io exitSuccess
    | x' =~ "^;uptime" = uptime >>= privmsg
    | x' =~ "^;id"     = privmsg $ drop 4 x'
    | x' =~ "hm"       = privmsg "."
    | x' =~ "\\?"      = privmsg "I sense a soul in search of answers."
    | x' =~ "^;ping"   = privmsg $ head (x =~ "^:(.*)!") !! 1 ++ " pinged."
    | otherwise        = return ()

    where
    x'    = clean x
    clean = drop 1 . dropWhile (/= ':') . drop 1


privmsg :: String -> Net ()
privmsg = write "PRIVMSG" . ((chan ++ " :") ++)


write :: String -> String -> Net ()
write s t = do
    h <- asks socket

    io $ hPrintf h "%s %s\r\n" s t
    io $ printf    "> %s %s\n" s t


uptime :: Net String
uptime = do
    now  <- io getClockTime
    zero <- asks starttime

    return . pretty $ diffClockTimes now zero

    where
    pretty td = unwords . map (uncurry (++) . first show) $
        if null diffs
        then [(0, "s")]
        else diffs

        where
        merge (tot, acc) (sec, typ) =
            let (sec', tot') = divMod tot sec
            in  (tot', (sec', typ) : acc)

        metrics =
            [ (86400, "d")
            , (3600,  "h")
            , (60,    "m")
            , (1,     "s") ]

        diffs = filter ((/= 0) . fst) . reverse . snd
              $ foldl' merge (tdSec td, []) metrics


    -- pretty td = join . intersperse " " . filter (not . null) . map f $
    --     [(years          ,"y") ,(months `mod` 12,"m")
    --     ,(days   `mod` 28,"d") ,(hours  `mod` 24,"h")
    --     ,(mins   `mod` 60,"m") ,(secs   `mod` 60,"s")]
    --     where
    --         secs    = abs $ tdSec td  ; mins   = secs   `div` 60
    --         hours   = mins   `div` 60 ; days   = hours  `div` 24
    --         months  = days   `div` 28 ; years  = months `div` 12
    --         f (i,s)
    --             | i == 0    = []
    --             | otherwise = show i ++ s


io :: IO a -> Net a
io = liftIO


type Net = ReaderT Bot IO
data Bot = Bot { socket :: Handle, starttime :: ClockTime }
