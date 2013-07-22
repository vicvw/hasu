module Main (main) where


import Canvas
import General
import Objects.Point
import Objects.Line


main :: IO ()
main = putStrLn $ render test
-- main = putStrLn . render $ empty (1000, 1000) (h '.')

    where
    test = draw
             [ point (x, 5) $ w c
             | (x, c) <- [17..] `zip` "いま"
             ]
         . draw -- 今
             [ line (18, 32) (06, 22) $ d ' ' -- /
             , line (18, 32) (30, 22) $ d ' ' -- \
             , line (14, 24) (21, 24) $ d ' ' -- -
             , line (11, 20) (24, 20) $ d ' ' -- --
             , line (24, 20) (19, 10) $ d ' ' -- /

             , line (18, 31) (06, 21) $ d '▒' -- /
             , line (18, 31) (30, 21) $ d '▒' -- \
             , line (14, 23) (21, 23) $ d '▒' -- -
             , line (11, 19) (24, 19) $ d '▒' -- --
             , line (24, 19) (19, 09) $ d '▒' -- /
             ]
         . draw -- い
             [ line (03, 12) (06, 06) $ d ' '
             , line (07, 06) (08, 07) $ d ' '
             , line (10, 13) (12, 10) $ d ' '
             , line (12, 09) (12, 08) $ d ' '

             , line (03, 11) (06, 05) $ d '▒'
             , line (07, 05) (08, 06) $ d '▒'
             , line (10, 12) (12, 09) $ d '▒'
             , line (12, 08) (12, 07) $ d '▒'
             ]
         . draw -- ま
             [ line (26, 12) (33, 12) $ d ' '
             , line (27, 10) (32, 10) $ d ' '
             , line (30, 14) (30, 06) $ d ' '
             , line (29, 05) (26, 06) $ d ' '
             , line (27, 07) (33, 05) $ d ' '

             , line (26, 11) (33, 11) $ d '▒'
             , line (27, 09) (32, 09) $ d '▒'
             , line (30, 13) (30, 05) $ d '▒'
             , line (29, 04) (26, 05) $ d '▒'
             , line (27, 06) (33, 04) $ d '▒'
             ]
         $ empty (36, 36) $ d '█'
