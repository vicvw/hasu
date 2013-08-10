module Omo.IO
    ( differentIO
    , fromArgs
    ) where


import Omo


import Control.Applicative  ((<$>))
import Control.Concurrent   (threadDelay)
import Control.Monad        (join, void)
import Data.List.Split      (splitOn)
import System.Cmd           (system)
import System.Environment   (getArgs)


differentIO :: DiffT (IO ()) -> IO ()
differentIO = join . different (return ())


fromArgs :: IO (DiffT (IO ()))
fromArgs
    = foldl1 (.)
    . map ((\[host, delay, cmd] -> toDiffT host delay cmd)
           . splitOn ":")
  <$> getArgs

    where
    toDiffT host delay cmd = ($ systemDelay delay' cmd) $
        case host of
        ""     -> 全
        "kaze" -> 風
        "sora" -> 空

        where
        delay' = case delay of
            "" -> 0
            _  -> read delay


systemDelay :: Int -> String -> IO ()
systemDelay delay cmd
    = threadDelay (delay * 10^6)
   >> void (system cmd)
