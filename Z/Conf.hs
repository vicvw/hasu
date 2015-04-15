module Conf
    ( conf
    ) where


import Z


conf :: [Expr]
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
    , alias "x"     "startx"
    , alias "r"     "ranger"

    , alias "l"     "tree --dirsfirst -ChL 1"
    , alias "lc"    "tree --dirsfirst -hL 1"
    , alias "l2"    "tree --dirsfirst -ChL 2"
    , alias "l3"    "tree --dirsfirst -ChL 3"
    , alias "la"    "l -a"
    , alias "lca"   "lc -a"
    , alias "ls"    "ls --color=auto"
    , alias "lsc"   "ls --color=never"
    , alias "lsa"   "ls -a"
    , alias "lsca"  "lsc -a"
    , alias "ll"    "ls -lh"
    , alias "lla"   "ls -lah"
    , alias "lld"   "ls -ldh"

    , alias "px"    "chmod +x"
    , alias "rf"    "rm -rf"
    , alias "cr"    "cp -r"
    , alias "c"     "cd"
    , alias "d"     "cd && clear"
    , alias "lns"   "ln -s"
    , alias "mv"    "mv -i"
    , alias "md"    "mkdir -p"
    , func  "mdc"
            [ "md $1 && cd $1" ]
    , alias "dus"   "du -hs"
    , alias "duu"   "du -hd 1"
    , alias "dh"    "df -h"
    , alias "hh"    "dh /dev/sda5"
    , alias "sw"    "s swapoff -a && s swapon -a"
    , alias "ti"    "dus ~/.local/share/Trash"
    , alias "trf"   "rf ~/.local/share/Trash"

    , alias "ctl"   "systemctl"
    , alias "sctl"  "s systemctl"

    , alias "dvd"   "s mount /dev/sr0 /media/dvd"
    , alias "udvd"  "s umount /media/dvd"

    , alias "pm"    "s pacman"
    , alias "pms"   "pm -S"
    , alias "pmr"   "pm -R"
    , alias "pmrs"  "pm -Rs"
    , alias "pmu"   "pm -U"
    , alias "pmss"  "pacman -Ss"
    , alias "pmsi"  "pacman -Si"
    , alias "pmqs"  "pacman -Qs"
    , alias "pmqi"  "pacman -Qi"
    , alias "pmqm"  "pacman -Qm"
    , alias "pmc"   "echo \"y\\nn\\n\" | s pacman -Scc"
    , alias "uu"    "packer --quickcheck"
    , alias "pa"    "packer"
    , alias "pas"   "packer -S"
    , alias "u"     "pa -Syu && pmc"
    , alias "uc"    "pa -Syu"

    , alias "o"     "mimeopen"
    , alias "v"     "vim"
    , alias "g"     "gvim"
    , alias "gr"    "gvim --remote-silent"
    , alias "sv"    "s vim"
    , alias "sg"    "s gvim"
    , alias "gi"    "git"
    , alias "e"     "evince"
    , alias "f"     "file-roller"
    , alias "m"     "mpv"
    , alias "vv"    "vlc"
    , alias "vino"  "killall compton; /usr/lib/vino/vino-server"
    , alias "lo"    "libreoffice"

    , alias "uz"    "unzip"
    , alias "ur"    "unrar x"
    , alias "ut"    "tar xf"


    , aliasS' "eog"
            [ "pdf"
            , "djvu"
            ]

    , aliasS' "eog"
            [ "jpg"
            , "jpeg"
            , "png"
            , "gif"
            ]

    , aliasS' "vv"
            [ "mp4"
            , "avi"
            , "mkv"
            ]

    , aliasS' "lo"
            [ "odt"
            , "ods"
            , "odp"
            , "doc"
            , "docx"
            , "ppt"
            , "pptx"
            ]

    , aliasS' "f"
            [ "zip"
            , "rar"
            , "tar"
            , "gz"
            , "bz2"
            , "xz"
            ]

    , aliasS "exe"  "wine"

    , alias "t"     "vitetris"
    , alias "ts"    "~/_/g/tetris/gameserver"

    , alias "mcs"   "cd ~/_/g/minecraft && java -Xmx1024M -Xms1024M -jar minecraft_server.jar nogui"

    , alias "h"     "ghci"
    , alias "rh"    "runhaskell"
    , alias "-s hs" "rh"
    , alias "ca"    "cabal"
    , alias "cau"   "ca update"
    , alias "cai"   "ca install"
    , alias "ht"    "cd ~/ふ/_; gvim _.hs; kaba -m _.hs"
    , func  "hp"
            [ "cd ~/ぶ/$1"
            , "gvim Main.hs"
            , "kaba -m Main.hs" ]

    , alias "j"     "java"
    , alias "jt"    "cd ~/ふ/_; gvim Test.java; kaba -m Test.java -i \"\\.class\""
    , alias "jc"    "javac"
    , alias "sc"    "scala"

    , alias "mozc"  "/usr/lib/mozc/mozc_tool --mode=config_dialog"

    , alias "db"    "dropbox-cli"
    , alias "dbs"   "dropbox-cli start"
    , alias "dbp"   "dropbox-cli stop"
    , alias "dbr"   "dropbox-cli stop && sleep 5 && dropbox-cli start"
    , alias "ds"    "dropbox-cli status"
    , alias "dsw"   "watch -n 1 dropbox-cli status"

    , alias "ci"    "s cpupower -c all frequency-info"
    , alias "cif"   "s cpupower -c all frequency-info | grep \"current CPU frequency\""
    , alias "cpo"   "s cpupower -c all frequency-set -g powersave"
    , alias "cpe"   "s cpupower -c all frequency-set -g performance"
    , alias "c8"    "s su -c \"echo 27 > /sys/devices/system/cpu/intel_pstate/max_perf_pct\""
    , alias "c14"   "s su -c \"echo 50 > /sys/devices/system/cpu/intel_pstate/max_perf_pct\""
    , alias "c0"    "s su -c \"echo 100 > /sys/devices/system/cpu/intel_pstate/max_perf_pct\""
    , alias "cit"   "cat /sys/devices/system/cpu/intel_pstate/no_turbo"
    , alias "ct"    "s su -c \"echo 0 > /sys/devices/system/cpu/intel_pstate/no_turbo\""
    , alias "cnt"   "s su -c \"echo 1 > /sys/devices/system/cpu/intel_pstate/no_turbo\""

    , alias "a"     "acpi"
    , alias "bat"   "cat /sys/class/power_supply/BAT0/charge_full"
    , alias "ptop"  "s powertop"
    , alias "hi"    "s hdparm -B /dev/sda"
    , alias "h0"    "s hdparm -B 255 /dev/sda"
    , alias "h1"    "s hdparm -B 1 /dev/sda"
    , alias "h128"  "s hdparm -B 128 /dev/sda"
    , alias "h254"  "s hdparm -B 254 /dev/sda"

    , alias "pp"    "ping g.co"

    , alias "uon"   "s vpnc uni.conf"
    , alias "uoff"  "s vpnc-disconnect"
    , alias "von"   "s openconnect --user=wiatrvic -b asa.rz.uni-augsburg.de"
    , alias "voff"  "s killall openconnect"
    , alias "wired" "snet uniw && von"

    , alias "gnome" "startx /usr/bin/gnome-session"
    , alias "ata"   "s catalyst_build_module all"
    , alias "mw"    "cd ~/.wine/drive_c/Program\\ Files/Bethesda\\ Softworks/Morrowind; wine Morrowind.exe; cd ~; clear"

    , alias "hib"   "systemctl hibernate"
    , alias "sus"   "systemctl suspend"
    , alias "ha"    "systemctl poweroff"
    , alias "re"    "systemctl reboot"

    , alias "k"     "eval `keychain --quiet --eval --agents ssh id_rsa`"
    , func  "kk"
            [ "eval `keychain --quiet --eval --agents ssh $1`" ]

    , func  ","
            [ clear
            , zshci "\"$@\"" ]

    , alias ",l"    ", l"

    , func  ".,"
            [ zshci "\"$@\""
            , clear ]

    , func  ",.,"
            [ clear
            , zshci "\"$@\""
            , clear ]


    , raw   "source" "~/.profile"
    , raw   "source" "~/.fzf.zsh"
    ]

    where
    clear = "clear"
    zshci = ("zsh -ci " ++)
