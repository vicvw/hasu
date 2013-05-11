module Keys
    ( getKey
    , Key
    ) where


import System.IO


data Key
    = Key0 | Key1 | Key2 | Key3 | Key4 | Key5 | Key6 | Key7 | Key8 | Key9
    | KeyA | KeyB | KeyC | KeyD | KeyE | KeyF | KeyG | KeyH | KeyI | KeyJ | KeyK | KeyL | KeyM | KeyN | KeyO | KeyP | KeyQ | KeyR | KeyS | KeyT | KeyU | KeyV | KeyW | KeyX | KeyY | KeyZ
    | KeyControl | KeyAlt


getKey :: IO Key
getKey = do
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False

    input <- getInput
    print input
    putStrLn "."

    return Key0

    where
    getInput = do
        char <- getChar
        let char' = [char]

        if char' `elem` ["\ACK", "\ESC"]
        then fmap ((char' ++) . (: "")) getChar
        else return char'


keyMap :: [(String, Key)]
keyMap =
    [ ("0", Key0)
    , ("1", Key1)
    , ("2", Key2)
    , ("3", Key3)
    , ("4", Key4)
    , ("5", Key5)
    , ("6", Key6)
    , ("7", Key7)
    , ("8", Key8)
    , ("9", Key9)
    ]
