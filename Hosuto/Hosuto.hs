module Hosuto
    ( actionPack
    , kazeX
    , soraX
    , allX

    , actionIO
    , actionX
    , spawnIO
    , spawnX
    , 風
    , 空
    ) where


import Control.Arrow          ((&&&))
import Control.Concurrent     (threadDelay)

-- import System.Process         (readProcess)

import Text.Printf            (printf)

import XMonad.Core            (io, X)
import XMonad.Util.SpawnOnce  (spawnOnce)


spawnIO :: ActionPack -> IO ()
spawnIO pack = do
    host <- id getHostname

    flip (maybe $ return ()) (($ pack) $ case host of
        "kaze" -> 風
        "sora" -> 空)
        $ uncurry (>>) .
            (threadDelay . floor . (* 10^6) . _delay
            &&&
            _actionIO . _action)


spawnX :: ActionPack -> X ()
spawnX pack = do
    host <- io getHostname

    flip (maybe . io $ return ()) (($ pack) $ case host of
        "kaze" -> 風
        "sora" -> 空)
        $ spawnOnce
        . uncurry shellSleep
        . (_delay &&&
           _actionX . _action)

        -- $ \(Action delay (ActionX action)) ->
        --     spawnOnce $ shellSleep delay action

        -- $ uncurry (>>) .
        --     (io . threadDelay . floor . (* 10^6) . _delay
        --     &&&
        --     spawnOnce . _actionX . _action)

    where
    shellSleep = printf "sh -c 'sleep %f; exec %s'"


getHostname :: IO String
getHostname = takeWhile (/= '\n')
       `fmap` readFile "/etc/hostname"
--        `fmap` readProcess "hostname" [] ""


kazeX, soraX, allX :: Double -> String -> ActionPack
allX delay cmd = actionPack
    { 風 = action
    , 空 = action
    }
    where
    action = Just $ actionX delay cmd

kazeX delay cmd = (allX delay cmd)
    { 空 = Nothing
    }

soraX delay cmd = (allX delay cmd)
    { 風 = Nothing
    }


actionPack :: ActionPack
actionPack = ActionPack
    { 風 = Nothing
    , 空 = Nothing
    }


data ActionPack = ActionPack
    { 風 :: Maybe Action
    , 空 :: Maybe Action
    }


actionIO :: Double -> IO () -> Action
actionIO delay = Action delay . ActionIO

actionX :: Double -> String -> Action
actionX delay = Action delay . ActionX


data Action = Action
    { _delay  :: Double
    , _action :: Hige
    }


data Hige
    = ActionIO { _actionIO :: (IO ()) }
    | ActionX  { _actionX  :: String  }
