module Options
    ( getOptions
    , oCommand
    , oFiles
    , oIgnored
    , oMain
    , oOnce
    ) where


import Data.List.Split    (splitOn)

import System.Console.GetOpt
import System.Environment (getArgs)


data Options = Options
    { oMain    :: Maybe String
    , oCommand :: Maybe String
    , oFiles   :: [String]
    , oIgnored :: [String]
    , oOnce    :: Bool
    } deriving Show


defaultOptions :: Options
defaultOptions = Options
    { oMain    = Nothing
    , oCommand = Nothing
    , oFiles   = []
    , oIgnored = []
    , oOnce    = False
    }


options :: [OptDescr (Options -> Options)]
options =
    [ Option "m" ["main"]
             (ReqArg (\file opts -> opts { oMain = Just file }) "FILE")
             "main file"

    , Option "c" ["cmd"]
             (ReqArg (\cmd opts -> opts { oCommand = Just cmd }) "COMMAND")
             "custom command"

    , Option "f" ["files"]
             (ReqArg (\files opts -> opts { oFiles = splitOn "," files }) "FILE,...")
             "watched files"

    , Option "i" ["ignore"]
             (ReqArg (\igns opts -> opts { oIgnored = splitOn "," igns }) "FILE,...")
             "unwatched files"

    , Option "1" ["once"]
             (NoArg (\opts -> opts { oOnce = True }))
             "run at least once"
    ]


getOptions :: IO (Options, [String])
getOptions = do
    args <- getArgs

    case getOpt Permute options args of
        (o, r, [])   -> return (foldl (flip id) defaultOptions o, r)
        (_, _, errs) -> ioError . userError
                      $ concat errs ++ usageInfo header options

    where
    header = "Usage: kaba [OPTION...] [-- [ARGUMENT...]]"
