module Syllabary
    ( hiragana
    , katakana
    , Syllabary
    ) where


hiragana :: Syllabary
hiragana =
    [ "a"   --> "あ"
    , "i"   --> "い"
    , "u"   --> "う"
    , "e"   --> "え"
    , "o"   --> "お"
    , "ka"  --> "か"
    , "ga"  --> "が"
    , "ki"  --> "き"
    , "kya" --> "きゃ"
    , "kyu" --> "きゅ"
    , "kyo" --> "きょ"
    , "gi"  --> "ぎ"
    , "gya" --> "ぎゃ"
    , "gyu" --> "ぎゅ"
    , "gyo" --> "ぎょ"
    , "ku"  --> "く"
    , "gu"  --> "ぐ"
    , "ke"  --> "け"
    , "ge"  --> "げ"
    , "ko"  --> "こ"
    , "go"  --> "ご"
    , "sa"  --> "さ"
    , "za"  --> "ざ"
    , "shi" --> "し"
    , "sha" --> "しゃ"
    , "shu" --> "しゅ"
    , "sho" --> "しょ"
    , "ji"  --> "じ"
    , "ja"  --> "じゃ"
    , "ju"  --> "じゅ"
    , "jo"  --> "じょ"
    , "su"  --> "す"
    , "zu"  --> "ず"
    , "se"  --> "せ"
    , "ze"  --> "ぜ"
    , "so"  --> "そ"
    , "zo"  --> "ぞ"
    , "ta"  --> "た"
    , "da"  --> "だ"
    , "chi" --> "ち"
    , "cha" --> "ちゃ"
    , "chu" --> "ちゅ"
    , "cho" --> "ちょ"
    , "ji"  --> "ぢ"
    , "ja"  --> "ぢゃ"
    , "ju"  --> "ぢゅ"
    , "jo"  --> "ぢょ"
    , "tsu" --> "つ"
    , "zu"  --> "づ"
    , "te"  --> "て"
    , "de"  --> "で"
    , "to"  --> "と"
    , "do"  --> "ど"
    , "na"  --> "な"
    , "ni"  --> "に"
    , "nya" --> "にゃ"
    , "nyu" --> "にゅ"
    , "nyo" --> "にょ"
    , "nu"  --> "ぬ"
    , "ne"  --> "ね"
    , "no"  --> "の"
    , "ha"  --> "は"
    , "ba"  --> "ば"
    , "pa"  --> "ぱ"
    , "hi"  --> "ひ"
    , "hya" --> "ひゃ"
    , "hyu" --> "ひゅ"
    , "hyo" --> "ひょ"
    , "bi"  --> "び"
    , "bya" --> "びゃ"
    , "byu" --> "びゅ"
    , "byo" --> "びょ"
    , "pi"  --> "ぴ"
    , "pya" --> "ぴゃ"
    , "pyu" --> "ぴゅ"
    , "pyo" --> "ぴょ"
    , "fu"  --> "ふ"
    , "bu"  --> "ぶ"
    , "pu"  --> "ぷ"
    , "he"  --> "へ"
    , "be"  --> "べ"
    , "pe"  --> "ぺ"
    , "ho"  --> "ほ"
    , "bo"  --> "ぼ"
    , "po"  --> "ぽ"
    , "ma"  --> "ま"
    , "mi"  --> "み"
    , "mya" --> "みゃ"
    , "myu" --> "みゅ"
    , "myo" --> "みょ"
    , "mu"  --> "む"
    , "me"  --> "め"
    , "mo"  --> "も"
    , "ya"  --> "や"
    , "yu"  --> "ゆ"
    , "yo"  --> "よ"
    , "ra"  --> "ら"
    , "ri"  --> "り"
    , "rya" --> "りゃ"
    , "ryu" --> "りゅ"
    , "ryo" --> "りょ"
    , "ru"  --> "る"
    , "re"  --> "れ"
    , "ro"  --> "ろ"
    , "wa"  --> "わ"
    , "wi"  --> "ゐ"
    , "wo"  --> "を"
    , "n"   --> "ん"
    , "vu"  --> "ゔ"
    , "~"   --> "〜"
    , ". "  --> "。"
    , ", "  --> "、"
    , ":"   --> "："
    , "!"   --> "！"
    , "?"   --> "？" ]
    ++
    ["k","s","t","p","m","r"] `zip` repeat "っ"


katakana :: Syllabary
katakana =
    [ "a"   --> "ア"
    , "i"   --> "イ"
    , "u"   --> "ウ"
    , "e"   --> "エ"
    , "o"   --> "オ"
    , "ka"  --> "カ"
    , "ga"  --> "ガ"
    , "ki"  --> "キ"
    , "kya" --> "キャ"
    , "kyu" --> "キュ"
    , "kyo" --> "キョ"
    , "gi"  --> "ギ"
    , "gya" --> "ギャ"
    , "gyu" --> "ギュ"
    , "gyo" --> "ギョ"
    , "ku"  --> "ク"
    , "gu"  --> "グ"
    , "ke"  --> "ケ"
    , "ge"  --> "ゲ"
    , "ko"  --> "コ"
    , "go"  --> "ゴ"
    , "sa"  --> "サ"
    , "za"  --> "ザ"
    , "shi" --> "シ"
    , "sha" --> "シャ"
    , "shu" --> "シュ"
    , "she" --> "シェ"
    , "sho" --> "ショ"
    , "ji"  --> "ジ"
    , "ja"  --> "ジャ"
    , "ju"  --> "ジュ"
    , "je"  --> "ジェ"
    , "jo"  --> "ジョ"
    , "su"  --> "ス"
    , "zu"  --> "ズ"
    , "se"  --> "セ"
    , "ze"  --> "ゼ"
    , "so"  --> "ソ"
    , "zo"  --> "ゾ"
    , "ta"  --> "タ"
    , "ti"  --> "ティ"
    , "da"  --> "ダ"
    , "di"  --> "ディ"
    , "du"  --> "ドゥ"
    , "chi" --> "チ"
    , "cha" --> "チャ"
    , "chu" --> "チュ"
    , "che" --> "チェ"
    , "cho" --> "チョ"
    , "ji"  --> "ヂ"
    , "ja"  --> "ヂャ"
    , "ju"  --> "ヂュ"
    , "jo"  --> "ヂョ"
    , "tsu" --> "ツ"
    , "tu"  --> "トゥ"
    , "zu"  --> "ヅ"
    , "te"  --> "テ"
    , "de"  --> "デ"
    , "to"  --> "ト"
    , "do"  --> "ド"
    , "na"  --> "ナ"
    , "ni"  --> "ニ"
    , "nya" --> "ニャ"
    , "nyu" --> "ニュ"
    , "nyo" --> "ニョ"
    , "nu"  --> "ヌ"
    , "ne"  --> "ネ"
    , "no"  --> "ノ"
    , "ha"  --> "ハ"
    , "ba"  --> "バ"
    , "pa"  --> "パ"
    , "hi"  --> "ヒ"
    , "hya" --> "ヒャ"
    , "hyu" --> "ヒュ"
    , "hyo" --> "ヒョ"
    , "bi"  --> "ビ"
    , "bya" --> "ビャ"
    , "byu" --> "ビュ"
    , "byo" --> "ビョ"
    , "pi"  --> "ピ"
    , "pya" --> "ピャ"
    , "pyu" --> "ピュ"
    , "pyo" --> "ピョ"
    , "fa"  --> "ファ"
    , "fi"  --> "フィ"
    , "fu"  --> "フ"
    , "fe"  --> "フェ"
    , "fo"  --> "フォ"
    , "bu"  --> "ブ"
    , "pu"  --> "プ"
    , "he"  --> "ヘ"
    , "be"  --> "ベ"
    , "pe"  --> "ペ"
    , "ho"  --> "ホ"
    , "bo"  --> "ボ"
    , "po"  --> "ポ"
    , "ma"  --> "マ"
    , "mi"  --> "ミ"
    , "mya" --> "ミャ"
    , "myu" --> "ミュ"
    , "myo" --> "ミョ"
    , "mu"  --> "ム"
    , "me"  --> "メ"
    , "mo"  --> "モ"
    , "ya"  --> "ヤ"
    , "yu"  --> "ユ"
    , "yo"  --> "ヨ"
    , "ra"  --> "ラ"
    , "ri"  --> "リ"
    , "rya" --> "リャ"
    , "ryu" --> "リュ"
    , "ryo" --> "リョ"
    , "ru"  --> "ル"
    , "re"  --> "レ"
    , "ro"  --> "ロ"
    , "wa"  --> "ワ"
    , "wi"  --> "ウィ"
    , "uu"  --> "ウ"
    , "we"  --> "ウェ"
    , "uo"  --> "ウォ"
    , "wo"  --> "ヲ"
    , "n"   --> "ン"
    , "va"  --> "ヴァ"
    , "vi"  --> "ヴィ"
    , "vu"  --> "ヴ"
    , "ve"  --> "ヴェ"
    , "vo"  --> "ヴォ"
    , "--"  --> "ー"
    , "~"   --> "〜"
    , " / " --> "・"
    , ". "  --> "。"
    , ", "  --> "、"
    , ":"   --> "："
    , "!"   --> "！"
    , "?"   --> "？" ]
    ++
    ["k","s","t","p","m","r"] `zip` repeat "ッ"


(-->) :: a -> b -> (a, b)
(-->) = (,)


type Syllabary = [(String, String)]
