module Pakkaa where


import Data.Maybe
import Text.Printf        (printf)
import Text.Regex.PCRE    ((=~))

import Control.Monad
import Control.Monad.Reader

import System.Process     (readProcess)
import System.Environment (getEnvironment)
import System.Posix.User  (getEffectiveUserID)
import System.Directory   --(createDirectoryIfMissing
                          --,removeDirectoryRecursive
                          --,getDirectoryContents)


main = do
    env  <- getEnvironment'
    home <- getHomeDirectory
    term <- getTerminalSize

    let pacman          = runReader getPacman env
        uid             = runReader getUid env

        -- tmpdir = printf "/tmp/pakkaatmp-%s" uid
        tmpdir          = printf "./pakkaatmp-%s" uid
        makepkgconf     = "/etc/makepkg.conf"
        usermakepkgconf = printf "%s/.makepkg.conf" home
        pacmanconf      = "/etc/pacman.conf"

        rpcurl          = "https://aur.archlinux.org/rpc.php?type"
        pkgcurl         = "https://aur.archlinux.org"

        (width, _)      = term


    mapM_ print [ pacman
                , uid
                , tmpdir
                , makepkgconf
                , usermakepkgconf
                , pacmanconf
                , rpcurl
                , pkgcurl ]

    print =<< getDirectoryContents "."
    -- createTmpDir tmpdir
    -- print =<< getDirectoryContents "."

    putStrLn $ "blub" `colored` Color6

    print width


createTmpDir :: FilePath -> IO ()
createTmpDir dir = do
    exists <- doesDirectoryExist dir
    when exists $ removeDirectoryRecursive dir
    createDirectoryIfMissing True dir


getPacman :: EnvironmentR
getPacman = return . fromMaybe "pacman" =<< asks (lookup "PACMAN")


getUid :: EnvironmentR
getUid = return . fromJust =<< asks (lookup "UID")


getEnvironment' :: IO Environment
getEnvironment' = do
    env <- getEnvironment
    uid <- getEffectiveUserID
    return $ ("UID", show uid) : env


getTerminalSize :: IO (Integer, Integer)
getTerminalSize = do
    resize <- readProcess "resize" [] []
    let (_, _, _, [w, h]) = resize =~ match :: (String, String, String, [String])
    return (read w, read h)
    where
    match = "COLUMNS=(\\d+).*\nLINES=(\\d+)"


colored :: String -> Color -> String
string `colored` color = printf "%s%s%s" color' string end
    where
    color' :: String
    color' = printf "\ESC[1;%sm" $ case color of
        Color1 -> "39"
        Color2 -> "32"
        Color3 -> "35"
        Color4 -> "36"
        Color5 -> "34"
        Color6 -> "33"
        Color7 -> "31"
    end = "\ESC[0m"


type Environment  = [(String, String)]
type EnvironmentR = Reader Environment String

data Color
    = Color1
    | Color2
    | Color3
    | Color4
    | Color5
    | Color6
    | Color7
