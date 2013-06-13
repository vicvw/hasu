module Main where


import System.Cmd     (system)
import System.IO      (BufferMode (..), hFlush, hSetBuffering, hSetEcho, stdin, stdout)
import System.Random  (randomRIO)


main :: IO ()
main = mainLoop
    where
    mainLoop = do
        kana <- randomKana hiragana
        putStrLn kana

        prompt ""

        doUntil (== kana)
                getInput
                prompt $
                \x -> do
                    prompt $ x ++ "\n"
                    system "clear"
                    mainLoop

        where
        prompt result = do
            putStr $ "\r> " ++ result
            hFlush stdout


doUntil :: Monad m => (t -> Bool) -> m t -> (t -> m a) -> (t -> m a) -> m a
doUntil p f false true = do
    y <- f

    if p y
    then true y
    else do
        false y
        doUntil p f false true


getInput :: IO String
getInput = do
    hSetEcho      stdin False
    hSetBuffering stdin NoBuffering

    input <- elemToList `fmap` getChar
    putStr " "

    return input


randomElement :: [a] -> IO a
randomElement xs = do
    index <- randomRIO (0, length xs - 1)

    return $ xs !! index


randomKana :: String -> IO String
randomKana = (elemToList `fmap`) . randomElement


elemToList :: a -> [a]
elemToList = (: [])


hiragana :: String
hiragana = concat
    [ "あいうえお"
    , "かきくけこ"
    , "がぎぐげご"
    , "さしすせそ"
    , "ざじずぜぞ"
    , "たちつてと"
    , "だぢづでど"
    , "なにぬねの"
    , "はひふへほ"
    , "ばびぶべぼ"
    , "ぱぴぷぺぽ"
    , "まみむめも"
    , "やゆよ"
    , "らりるれろ"
    , "わを"
    ]


katakana :: String
katakana = concat
    [ "アイウエオ"
    , "カキクケコ"
    , "ガギグゲゴ"
    , "サシスセソ"
    , "ザジズゼゾ"
    , "タチツテト"
    , "ダヂヅデド"
    , "ナニヌネノ"
    , "ハヒフヘホ"
    , "バビブベボ"
    , "パピプペポ"
    , "マミムメモ"
    , "ヤユヨ"
    , "ラリルレロ"
    , "ワヲ"
    ]
