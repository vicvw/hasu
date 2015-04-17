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
        , "d"     ｜  "cd && clear"
        , "px"    ｜  "chmod +x"
        , "rf"    ｜  "rm -rf"
        , "cr"    ｜  "cp -r"
        , "mv"    ｜  "mv -i"
        , "md"    ｜  "mkdir -p"
        , "lns"   ｜  "ln -s"

        , "l"     ｜  tree "-ChL 1"
        , "l2"    ｜  tree "-ChL 2"
        , "l3"    ｜  tree "-ChL 3"
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
        , "sw"    ｜  unwords [s "swapoff -a", "&&", s "swapon -a"]
        , "trf"   ｜  "rf ~/.local/share/Trash"

        , "ctl"   ｜  "systemctl"
        , "sctl"  ｜  s "systemctl"

        , "dvd"   ｜  s "mount /dev/sr0 /media/dvd"
        , "udvd"  ｜  s "umount /media/dvd"

        , "pa"    ｜       "packer"
        , "pas"   ｜  pa   "-S"
        , "pm"    ｜  s    "pacman"
        , "pms"   ｜  spm  "-S"
        , "pmr"   ｜  spm  "-R"
        , "pmrs"  ｜  spm  "-Rs"
        , "pmu"   ｜  spm  "-U"
        , "pmss"  ｜  pm   "-Ss"
        , "pmsi"  ｜  pm   "-Si"
        , "pmqs"  ｜  pm   "-Qs"
        , "pmqi"  ｜  pm   "-Qi"
        , "pmqm"  ｜  pm   "-Qm"
        , "uu"    ｜  pa   "--quickcheck"
        , "uc"    ｜  pa   "-Syu"
        , "u"     ｜  unwords [pa "-Syu", "&&", pmClear]
        , "pmc"   ｜  pmClear

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

        , "ap"    ｜  "apack"
        , "au"    ｜  "aunpack"
        , "uz"    ｜  "unzip"
        , "ur"    ｜  "unrar x"
        , "ut"    ｜  "tar xf"

        , "t"     ｜  "vitetris"
        , "ts"    ｜  "~/_/g/tetris/gameserver"

        , "mcs"   ｜  "cd ~/_/g/minecraft && java -Xmx1024M -Xms1024M -jar minecraft_server.jar nogui"

        , "h"     ｜  "ghci"
        , "rh"    ｜  "runhaskell"
        , "-s hs" ｜  "rh"
        , "ca"    ｜  "cabal"
        , "cau"   ｜  ca "update"
        , "cai"   ｜  ca "install"
        , "ht"    ｜  "cd ~/ふ/_; gvim _.hs; kaba -m _.hs"

        , "j"     ｜  "java"
        , "jc"    ｜  "javac"
        , "jt"    ｜  "cd ~/ふ/_; gvim Test.java; kaba -m Test.java -i \"\\.class\""
        , "sc"    ｜  "scala"

        , "mozc"  ｜  "/usr/lib/mozc/mozc_tool --mode=config_dialog"

        , "db"    ｜  "dropbox-cli"
        , "dbs"   ｜  dbc "start"
        , "dbp"   ｜  dbc "stop"
        , "dbr"   ｜  unwords [dbc "stop", "&& sleep 5 &&", dbc "start"]
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

        , "k"     ｜  "eval `keychain --quiet --eval --agents ssh id_rsa`"

        , ",l"    ｜  ", l"
        ]


    , aliasS''
        [ "evince" ｜
            [ "pdf"
            , "djvu"
            ]

        , "eog" ｜
            [ "jpg"
            , "jpeg"
            , "png"
            , "gif"
            ]

        , "vlc" ｜
            [ "mp4"
            , "avi"
            , "mkv"
            ]

        , "libreoffice" ｜
            [ "odt"
            , "ods"
            , "odp"
            , "doc"
            , "docx"
            , "ppt"
            , "pptx"
            ]

        , "file-roller" ｜
            [ "zip"
            , "rar"
            , "tar"
            , "gz"
            , "bz2"
            , "xz"
            ]

        , "wine" ｜
            [ "exe" ]
        ]


    , func'
        [ "mdc" ｜
            [ "md $1 && cd $1" ]

        , "hp" ｜
            [ "cd ~/ぶ/$1"
            , "gvim Main.hs"
            , "kaba -m Main.hs" ]

        , "kk" ｜
            [ "eval `keychain --quiet --eval --agents ssh $1`" ]

        , "," ｜
            [ clear
            , zshci $ q "$@" ]

        , ".," ｜
            [ zshci $ q "$@"
            , clear ]

        , ",.," ｜
            [ clear
            , zshci $ q "$@"
            , clear ]
        ]


    , raw' $ map ("source" ｜)
        [ "~/.profile"
        , "~/.fzf.zsh"
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
    ls    = pre "ls --color=auto"
    lsc   = pre "ls --color=never"
    du    = pre "du"
    dus   = pre $ du "-hs"
    df    = pre "df"
    l     = pre $ tree "ChL 1"
    lc    = pre $ tree "hL 1"
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

    pre s   = unwords . ([s] ++) . return

    infixr 9 ｜
    (｜)    = (,)
