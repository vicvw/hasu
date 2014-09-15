module Main (main) where


import Control.Applicative  ((<$>))
import Control.Monad        (sequence, void, when)

import System.Exit          (ExitCode (..))
import System.Process       (readProcess, system)


main :: IO ()
main = do
    success <- and <$> sequence [toggleSawa, toggleMouse, not <$> sawaRunning]

    when success . void $
        system "sleep 1; xset dpms force off"


toggleMouse :: IO Bool
toggleMouse = successfully <$> ifM disable enable isEnabled

    where
    enable  = able "1"
    disable = able "0"
    able    = system . ("xinput set-prop 10 'Device Enabled' " ++)

    isEnabled = (<$> readProcess "xinput" ["list-props", "10"] "") $ \out ->
        case last $ lines out !! 1 of
            '1' -> True
            '0' -> False


toggleSawa :: IO Bool
toggleSawa = successfully <$> ifM kill spawn sawaRunning

    where
    spawn = system "sawa&"
    kill  = system "killall sawa"


sawaRunning :: IO Bool
sawaRunning = successfully <$> system "pidof sawa"


successfully :: ExitCode -> Bool
successfully ExitSuccess  = True
successfully _            = False


ifM :: Monad m => m a -> m a -> m Bool -> m a
ifM true false condM = (condM >>=) $ \cond ->
    if cond
    then true
    else false
