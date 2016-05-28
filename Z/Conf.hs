module Conf
    ( conf
    ) where


import Z


conf :: [Expr]
conf =
    [ var'
        [ "ZSH"                        ｜ "$HOME/.oh-my-zsh"
        , "DISABLE_AUTO_UPDATE"        ｜ "true"
        , "DISABLE_AUTO_TITLE"         ｜ "true"
        , "plugins"                    ｜ "(git zsh-syntax-highlighting)"
        , "ZSH_HIGHLIGHT_HIGHLIGHTERS" ｜ "(main brackets pattern)"
        ]

    , varEx "ZSH_THEME" "kaze"

    , raw'
        [ "source" ｜ "$ZSH/oh-my-zsh.sh"
        , "setopt" ｜ unwords
              [ "appendhistory"
              , "autocd"
              , "extendedglob"
              , "nomatch"
              , "notify"
              , "histignorealldups"
              ]
        , "unsetopt" ｜ unwords
              [ "correct_all"
              , "beep"
              , "histignorespace"
              , "share_history"
              ]
        , "bindkey"  ｜ "-v"
        , "autoload" ｜ "-Uz compinit"
        , "compinit" ｜ ""
        , "unset"    ｜ "GREP_OPTIONS"
        ]


    , var'
        [ "HISTFILE"   ｜ "~/.histfile"
        , "HISTSIZE"   ｜ "1000"
        , "SAVEHIST"   ｜ "1000"
        , "KEYTIMEOUT" ｜ "1"
        ]


    , alias'
        [ "sr"    ｜  "source ~/.zshrc"

        , "lang"  ｜  "echo $LANG"
        , "en"    ｜  lang "en_US"
        , "ja"    ｜  lang "ja_JP"
        , "ko"    ｜  lang "ko_KR"
        , "tw"    ｜  lang "zh_TW"

        , "s"     ｜  "sudo"
        , "r"     ｜  "ranger"
        , "c"     ｜  "cd"
        , "d"     ｜  "cd" ＆ clear
        , "px"    ｜  "chmod +x"
        , "rf"    ｜  "rm -rf"
        , "cr"    ｜  "cp -r"
        , "mv"    ｜  "mv -i"
        , "md"    ｜  "mkdir -p"
        , "lns"   ｜  "ln -s"

        , "l"     ｜  treeL 1
        , "l2"    ｜  treeL 2
        , "l3"    ｜  treeL 3
        , "l4"    ｜  treeL 4
        , "l5"    ｜  treeL 5
        , "lc"    ｜  tree "-hL 1"
        , "la"    ｜  l    "-a"
        , "lca"   ｜  lc   "-a"
        , "ls"    ｜       "ls --color=auto"
        , "lsc"   ｜       "ls --color=never"
        , "lsa"   ｜  ls   "-a"
        , "lsca"  ｜  lsc  "-a"
        , "ll"    ｜  ls   "-lh"
        , "lla"   ｜  ls   "-lah"
        , "lld"   ｜  ls   "-ldh"

        , "dus"   ｜  du   "-hs"
        , "duu"   ｜  du   "-hd 1"
        , "dh"    ｜  df   "-h"
        , "hh"    ｜  df   "-h /dev/sda5"
        , "ti"    ｜  dus  "~/.local/share/Trash"
        , "sw"    ｜  s "swapoff -a" ＆ s "swapon -a" ＆ clear
        , "trf"   ｜  "rf ~/.local/share/Trash"

        , "ctl"   ｜  "systemctl"
        , "sctl"  ｜  s "systemctl"

        , "nr"    ｜  s "netctl restart"
        , "nrr"   ｜  s "netctl stop-all" ＆ s "netctl start"

        , "dvd"   ｜  s "mount /dev/sr0 /media/dvd"
        , "udvd"  ｜  s "umount /media/dvd"

        , "pa"    ｜      "packer"
        , "pas"   ｜  pa  "-S"
        , "pm"    ｜  s   "pacman"
        , "pms"   ｜  spm "-S"
        , "pmr"   ｜  spm "-R"
        , "pmrs"  ｜  spm "-Rs"
        , "pmu"   ｜  spm "-U"
        , "pmss"  ｜  pm  "-Ss"
        , "pmsi"  ｜  pm  "-Si"
        , "pmqs"  ｜  pm  "-Qs"
        , "pmqi"  ｜  pm  "-Qi"
        , "pmqm"  ｜  pm  "-Qm"
        , "uu"    ｜  pa  "--quickcheck"
        , "uc"    ｜  pa  "-Syu"
        , "u"     ｜  pa  "-Syu" ＆ pmClear
        , "pmc"   ｜  pmClear
        , "rp"    ｜  "rf /tmp/packerbuild-1000"

        , "o"     ｜  "mimeopen"
        , "v"     ｜  "vim"
        , "g"     ｜  "gvim"
        , "gr"    ｜  "gvim --remote-silent"
        , "sv"    ｜  s "vim"
        , "sg"    ｜  s "gvim"
        , "gi"    ｜  "git"
        , "e"     ｜  "evince"
        , "f"     ｜  "file-roller"
        , "m"     ｜  "mpv"
        , "vv"    ｜  "vlc"
        , "lo"    ｜  "libreoffice"
        , "pp"    ｜  "ping g.co"
        , "vino"  ｜  "killall compton; /usr/lib/vino/vino-server"
        , "ge"    ｜  "geki \"\"" ＆ clear

        , "ap"    ｜  "apack"
        , "au"    ｜  "aunpack"
        , "uz"    ｜  "unzip"
        , "ur"    ｜  "unrar x"
        , "ut"    ｜  "tar xf"

        , "t"     ｜  "vitetris"
        , "ts"    ｜  "~/_/g/tetris/gameserver"
        , "mcs"   ｜  "cd ~/_/g/minecraft" ＆ "java -Xmx1024M -Xms1024M -jar minecraft_server.jar nogui"

        , "h"     ｜  "ghci"
        , "rh"    ｜  "runhaskell"
        , "ca"    ｜  "cabal"
        , "cau"   ｜  ca "update"
        , "cai"   ｜  ca "install"
        , "ht"    ｜  "cd ~/ふ/_; gvim _.hs; k -m _.hs"

        , "j"     ｜  "java"
        , "jc"    ｜  "javac"
        , "jt"    ｜  "cd ~/ふ/_; gvim Test.java; k -m Test.java -i \".class\""
        , "sc"    ｜  "scala"

        , "mozc"  ｜  "/usr/lib/mozc/mozc_tool --mode=config_dialog"

        , "db"    ｜  "dropbox-cli"
        , "dbs"   ｜  dbc "start" ＆ clear
        , "dbb"   ｜  dbc "stop" ＆ clear
        , "dbr"   ｜  dbc "stop" ＆ "sleep 5" ＆ dbc "start"
        , "ds"    ｜  dbc "status"
        , "dsw"   ｜  pre "watch -n 1" (dbc "status")

        , "ci"    ｜  cpu "frequency-info"
        , "cif"   ｜  cpu "frequency-info | grep \"current CPU frequency\""
        , "cpo"   ｜  cpu "frequency-set -g powersave"
        , "cpe"   ｜  cpu "frequency-set -g performance"
        , "c27"   ｜  su (cpuF 27)
        , "c50"   ｜  su (cpuF 50)
        , "c00"   ｜  su (cpuF 100)
        , "ct"    ｜  su (cpuT True)
        , "cnt"   ｜  su (cpuT False)
        , "cit"   ｜  cat noTurbo

        , "a"     ｜       "acpi"
        , "ptop"  ｜  s    "powertop"
        , "bat"   ｜  cat  "/sys/class/power_supply/BAT0/charge_full"
        , "hi"    ｜  hdB  "/dev/sda"
        , "h0"    ｜  hdB  "255 /dev/sda"
        , "h1"    ｜  hdB  "1 /dev/sda"
        , "h28"   ｜  hdB  "128 /dev/sda"
        , "h54"   ｜  hdB  "254 /dev/sda"

        , "uon"   ｜  s "vpnc uni.conf"
        , "uoff"  ｜  s "vpnc-disconnect"
        , "von"   ｜  s "openconnect --user=wiatrvic -b asa.rz.uni-augsburg.de"
        , "voff"  ｜  s "killall openconnect"

        , "gnome" ｜  "startx /usr/bin/gnome-session"
        , "mw"    ｜  "cd ~/.wine/drive_c/Program\\ Files/Bethesda\\ Softworks/Morrowind; wine Morrowind.exe; cd ~; clear"
        , "ata"   ｜  s "catalyst_build_module all"

        , "hib"   ｜  ctl "hibernate"
        , "sus"   ｜  ctl "suspend"
        , "ha"    ｜  ctl "poweroff"
        , "re"    ｜  ctl "reboot"

        , "kc"    ｜  "eval `keychain --quiet --eval --agents ssh id_rsa`"

        , ",l"    ｜  ", l"
        ]


    , aliasS''
        [ "mimeopen" ｜
            [ "pdf"
            , "djvu"

            , "jpg"
            , "jpeg"
            , "png"
            , "gif"
            , "bmp"

            , "mp3"
            , "mp4"
            , "m4a"
            , "avi"
            , "mkv"

            , "odt"
            , "ods"
            , "odp"
            , "doc"
            , "docx"
            , "ppt"
            , "pptx"

            , "zip"
            , "rar"
            , "tar"
            , "gz"
            , "bz2"
            , "xz"

            , "html"

            , "exe"
            , "EXE"
            ]

        , "runhaskell" ｜
            [ "hs" ]
        ]


    , func'
        [ "command_not_found_handler" ｜
            [ "google-chrome-stable \"google.de/search?q=$*\" &> /dev/null &|"
            ]

        , "oki" ｜
            [ s "rtcwake -m \"$1\" -t $(date +%s -d \"$2\")"
            , "raku c play"
            ]

        , "mdc" ｜
            [ "md $1 && c $1" ]

        , "hp" ｜
            [ "c ~/ぶ/$1"
            , "g Main.hs"
            , "k -m Main.hs"
            ]

        , "kk" ｜
            [ "eval `keychain --quiet --eval --agents ssh $1`" ]

        , "," ｜
            [ clear
            , zshci $ q "$@"
            ]

        , ".," ｜
            [ zshci $ q "$@"
            , clear
            ]

        , ",.," ｜
            [ clear
            , zshci $ q "$@"
            , clear
            ]

        , "zc" ｜
            [ "c ~/ぶ/Z"
            , "g Conf.hs"
            , "k -m Main.hs"
            ]

        , "zg" ｜
            [ "c ~/ぶ/Z"
            , "rh Main.hs > ~/.zshrc"
            , "c -"
            ]
        ]


    , raw' $ map ("source" ｜)
        [ "~/.profile"
        , "~/.dot/fzf.zsh"
        ]
    ]

    where
    lang  = ("LANG=" ++) . q . (++ ".UTF-8")
    s     = pre "sudo"
    ctl   = pre "systemctl"
    cat   = pre "cat"
    pm    = pre "pacman"
    spm   = pre "sudo pacman"
    pa    = pre "packer"
    ca    = pre "cabal"
    dbc   = pre "dropbox-cli"
    tree  = pre "tree --dirsfirst"
    treeL = pre (tree "-ChL") . show
    ls    = pre "ls --color=auto"
    lsc   = pre "ls --color=never"
    du    = pre "du"
    dus   = pre $ du "-hs"
    df    = pre "df"
    l     = pre $ tree "-ChL 1"
    lc    = pre $ tree "-hL 1"
    cpu   = pre $ s "cpupower -c all"
    su    = pre $ s "su -c"
    hdB   = pre $ s "hdparm -B"

    clear = "clear"
    zshci = ("zsh -ci " ++)

    cpuF f = q $ unwords ["echo", show f, ">", maxPerf]
    cpuT b = q $ unwords ["echo", b',     ">", noTurbo]
        where
        b' = if b then "0" else "1"

    pmClear = "echo \"y\\nn\\n\" | s pacman -Scc"
    maxPerf = "/sys/devices/system/cpu/intel_pstate/max_perf_pct"
    noTurbo = "/sys/devices/system/cpu/intel_pstate/no_turbo"

    pre s   = unwords . (s :) . return

    infixr 9 ＆, ｜
    a ＆ b  = unwords [a, "&&", b]
    (｜)    = (,)
