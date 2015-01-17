module Conf
    ( conf
    ) where


import Z


conf :: [ZExpr]
conf =
    [ var   "ZSH"       "$HOME/.oh-my-zsh"
    , varEx "ZSH_THEME" "kaze"

    , var   "DISABLE_AUTO_UPDATE" "true"
    , var   "DISABLE_AUTO_TITLE"  "true"

    , var   "plugins" "(git zsh-syntax-highlighting)"

    , var   "ZSH_HIGHLIGHT_HIGHLIGHTERS"
            "(main brackets pattern)"

    , raw   "source" "$ZSH/oh-my-zsh.sh"
    , raw   "setopt" $ unwords
            [ "appendhistory"
            , "autocd"
            , "extendedglob"
            , "nomatch"
            , "notify"
            , "histignorealldups"
            ]
    , raw   "unsetopt" $ unwords
            [ "correct_all"
            , "beep"
            , "histignorespace"
            , "share_history"
            ]
    , raw   "bindkey"  "-v"
    , raw   "autoload" "-Uz compinit"
    , raw   "compinit" ""


    , var   "HISTFILE"    "~/.histfile"
    , var   "HISTSIZE"    "1000"
    , var   "SAVEHIST"    "1000"
    , var   "KEYTIMEOUT"  "1"


    , raw   "unset" "GREP_OPTIONS"

    , alias "sr"    "source ~/.zshrc"

    , alias "lang"  "echo $LANG"
    , alias "en"    "LANG=\"en_US.UTF-8\""
    , alias "ja"    "LANG=\"ja_JP.UTF-8\""
    , alias "ko"    "LANG=\"ko_KR.UTF-8\""
    , alias "tw"    "LANG=\"zh_TW.UTF-8\""

    , alias "s"     "sudo"

    , alias "ctl"   "systemctl"
    , alias "sctl"  "s ctl"
    ]
