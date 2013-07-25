module Conf
    ( conf
    ) where


import Z


conf :: [ZShellExpr]
conf =
    [ varApp "PATH"     "~/,,/bin"            []
    , varApp "PATH"     "~/.cabal/bin"        ["PATH"]
    , export "PATH"

    , var     "LANG"    "ja_JP.UTF-8"         []
    , export  "LANG"

    , var     "EDITOR"  "vim"                 []
    , export  "EDITOR"

    , alias   "lang"    "echo $LANG"          ["export LANG"]
    , alias   "en"      "LANG='en_US.UTF-8'"  []
    , alias   "ja"      "LANG='ja_JP.UTF-8'"  []

    , alias   "sr"      "source ~/.zshrc"     []
    , alias   "s"       "sudo"                []
    , alias   "x"       "startx"              []
    , alias   "ctl"     "systemctl"           []
    , alias   "sctl"    "s systemctl"         ["s"]

    , alias   "ls"      "ls --color=auto"     []
    , alias   "la"      "ls -a"               ["ls"]
    , alias   "ll"      "ls -l"               ["ls"]
    , alias   "lla"     "ls -la"              ["ls"]
    , alias   "lld"     "ls -ld"              ["ls"]

    , alias   "px"      "chmod +x"            []
    , alias   "rf"      "rm -rf"              []
    , alias   "cr"      "cp -r"               []
    , alias   "c"       "cd"                  []
    , alias   "d"       "cd && clear"         []
    , alias   "lns"     "ln -s"               []
    , alias   "md"      "mkdir -p"            []

    , alias   "dvd"     "s mount /dev/sr0 /media/dvd" ["s"]
    , alias   "udvd"    "s umount /media/dvd" ["s"]

    , alias   "pa"      "packer"              []
    , alias   "pas"     "pa -S"               ["pa"]
    , alias   "pmn"     "pacman"              []
    , alias   "pm"      "s pacman"            ["s"]
    , alias   "pms"     "pms -S"              ["pms"]
    , alias   "pmss"    "pmn -Ss"             ["pmn"]
    , alias   "pmqs"    "pmn -Qs"             ["pmn"]
    , alias   "pmqi"    "pmn -Qi"             ["pmn"]
    , alias   "pmqm"    "pmn -Qm"             ["pmn"]
    , alias   "pmr"     "pms -R"              ["pms"]
    , alias   "pmc"     "echo 'y\\nn\\n' | pms -Scc" ["pms"]
    , alias   "uc"      "pa -Syu"             ["pa"]
    , alias   "u"       "uc && pmc"           ["uc", "pmc"]

    , alias   "o"       "xdg-open"            []
    , alias   "v"       "vim"                 []
    , alias   "gv"      "gvim"                []
    , alias   "sv"      "s vim"               ["s"]
    , alias   "sgv"     "s gvim"              ["s"]
    , alias   "g"       "gv --remote-silent"  ["gv"]
    , alias   "gi"      "git"                 []
    , alias   "e"       "evince"              []
    , alias   "f"       "file-roller"         []
    , alias   "m"       "mplayer"             []
    , alias   "lo"      "libreoffice"         []
    , alias   "we"      "weechat-curses"      []
    , alias   "vino"    "killall xcompmgr; /usr/lib/vino/vino-server" []
    , alias   "trayer"  "trayer --edge top --align right --SetDockType true --widthtype request --transparent true --alpha 255 --padding 5" []

    , aliasS  "zshrc"   "v"                   ["v"]
    , aliasS  "vimrc"   "v"                   ["v"]

    , aliasS  "pdf"     "e"                   ["e"]
    , aliasS  "jpg"     "eog"                 []
    , aliasS  "jpeg"    "eog"                 []
    , aliasS  "png"     "eog"                 []
    , aliasS  "gif"     "eog"                 []

    , aliasS  "odt"     "lo"                  ["lo"]
    , aliasS  "ods"     "lo"                  ["lo"]
    , aliasS  "odp"     "lo"                  ["lo"]
    , aliasS  "doc"     "lo"                  ["lo"]
    , aliasS  "docx"    "lo"                  ["lo"]
    , aliasS  "ppt"     "lo"                  ["lo"]
    , aliasS  "pptx"    "lo"                  ["lo"]

    , aliasS  "zip"     "f"                   ["f"]
    , aliasS  "rar"     "f"                   ["f"]
    , aliasS  "tar"     "f"                   ["f"]
    , aliasS  "gz"      "f"                   ["f"]
    , aliasS  "bz2"     "f"                   ["f"]

    , aliasS  "exe"     "wine"                []
    ]
