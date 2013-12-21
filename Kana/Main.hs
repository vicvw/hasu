module Main where


import Control.Applicative  ((<$>))

import System.Cmd           (system)
import System.IO            (BufferMode (..), hFlush, hSetBuffering, hSetEcho, stdin, stdout)
import System.Random        (randomRIO)


main :: IO ()
main = mainLoop
    where
    mainLoop = do
        -- kana <- randomKana [平仮名, 平仮名濁点]
        hangeul <- randomFromList $ concat [한글]
        putStrLn hangeul

        prompt ""

        doUntil (== hangeul)
                getInput
                prompt $ \x -> do
                    prompt $ x ++ "\n"
                    system "clear"
                    mainLoop

        where
        prompt result = do
            putStr $ "\r仮名 " ++ result
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

    input <- getChar
    putStr " "

    return $ return input


randomFromList :: String -> IO String
randomFromList = (return <$>) . randomElement

    where
    randomElement xs = (xs !!) <$> randomRIO (0, length xs - 1)


平仮名, 平仮名濁点, 他の平仮名 :: String
平仮名 = concat
    [ "あいうえお"
    , "かきくけこ"
    , "さしすせそ"
    , "たちつてと"
    , "なにぬねの"
    , "はひふへほ"
    , "まみむめも"
    , "やゆよ"
    , "らりるれろ"
    , "わを"
    ]

平仮名濁点 = concat
    [ "がぎぐげご"
    , "ざじずぜぞ"
    , "だぢづでど"
    , "ばびぶべぼ"
    , "ぱぴぷぺぽ"
    ]

他の平仮名 = concat
    [ "ぁぃぅぇぉ"
    , "っゎ"
    ]


片仮名, 片仮名濁点, 他の片仮名 :: String
片仮名 = concat
    [ "アイウエオ"
    , "カキクケコ"
    , "サシスセソ"
    , "タチツテト"
    , "ナニヌネノ"
    , "ハヒフヘホ"
    , "マミムメモ"
    , "ヤユヨ"
    , "ラリルレロ"
    , "ワヲ"
    ]

片仮名濁点 = concat
    [ "ガギグゲゴ"
    , "ザジズゼゾ"
    , "ダヂヅデド"
    , "バビブベボ"
    , "パピプペポ"
    ]

他の片仮名 = concat
    [ "ァィゥェォ"
    , "ッヮ"
    ]


他 :: String
他 = concat
    [ "ヵヶー"
    , "、。・"
    ]


한글 :: String
한글 = concat
    [ "ㄱㄲㅋㅂㅃㅍㅅㅆㅈㅉㅊㄷㄸㅌㅁㄴㅇㄹㅎ"
    , "ㅗㅛㅏㅑㅓㅕㅜㅠㅐㅒㅔㅖㅣㅡ"
    ]
