module Dzen.Notification
    (
    ) where


import Dzen.Color
import Dzen.Dzen

import Control.Monad  (void)
import System.Process (system)


main :: IO ()
main = notify "blub"


notify :: String -> IO ()
notify message
    = void
    . system
    $ ("echo " ++ message)
  <|> dzen'

    where
    dzen' = dzen
        [ Timeout     2
        , Height      20
        , Width       1920
        , XPosition   0
        , YPosition   1080
        , Background  $ red 150
        , Foreground  $ grey 250
        , Font        "UmePlus P Gothic:size=12"
        -- , TitleAlignment ALeft
        ]
