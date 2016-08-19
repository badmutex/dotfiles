{ pkgs
, stdenv
, ...
}:

with pkgs;

let

  inherit (stdenv.lib) optional optionals;
  inherit (stdenv) isLinux;
  myOptions = callPackage ./packageChoices.nix { };

  args = {inherit pkgs config stdenv;} // myOptions;

  gitchangelog = callPackage ./apps/gitchangelog.nix { inherit (pkgs.python27Packages) d2to1; };
  devPython27 = callPackage ./devenv/python27.nix {};
  devHaskell  = with args; callPackage ./devenv/haskell.nix {};

in

let

  all = with args;

    ### cloudfs / synchronization
       optional withDropbox dropbox
    ++ optional withMega    megatools
    ++ optional isLinux      unison

    ### development
    ++ [ emacs autoconf gnumake gcc silver-searcher colordiff ]

    ### development / git tools
    ++ (with gitAndTools;
       [ git hub git-extras git-remote-hg
         git2cl gitFastExport gitflow topGit
         gitchangelog
       ])

    # ### development / python
    ++ [ devPython27 ]
    ++ [ devHaskell  ]

    # ### finance
    ++ (with haskellPackages;
       [ hledger hledger-ui hledger-web hledger-diff ])

    ### games
    ++ optional withWesnoth wesnoth

    ### monitoring
    ++ optionals isLinux [ iotop htop nethogs ]

    ### media
    ++ optional withSpotify spotify
    ++ [vlc smplayer mplayer mpv]

    ### nix
    ++ [ nix-repl ]

    ### office / productivity
    ++ optional  withEvince evince
    ++ optional  withInkscape    inkscape
    ++ optionals withLatex       [texlive.combined.scheme-full biber ghostscript]
    ++ optional  withLibreOffice libreoffice
    ++ optional  withPopfile     popfile

    ### security
    ++ [ gnupg gnutls pinentry keychain keepassx2 ]
    ++ optional  isLinux paperkey
    ++ optionals withYubikey [ yubikey-personalization-gui
                               yubikey-personalization ]

    ### tools
    ++ [ aspell aspellDicts.en
         bc
         graphviz-nox
         imagemagick
         inotify-tools
         most
         tree
         unison
         unzipNLS
         zip
       ]
    ++ optional isLinux aria

    ### web / network
    ++ [ httpie jq nmap w3m ]
    ++ optional withChrome   google-chrome
    ++ optional withChromium chromium

    ### X11, window management
    ++ [ screen tmux ]                     # terminal multiplexers
    ++ [ enlightenment.terminology ]       # terminals
    ++ [ xclip xdotool                     # X management
         gnome3.gnome_keyring ]
    ++ [ feh rofi ]                        # X tools
    ++ (with xorg; [xev xbacklight ])
    ++ [ networkmanagerapplet pa_applet    # applets
         parcellite ]

    ### XMonad
    ++ [ dmenu trayer compton haskellPackages.xmobar ]

    ;

in {
  badi = buildEnv {
    name = "badi-packages";
    paths =  all;
  };
  badi-recordings = buildEnv {
    name = "badi-recordings";
    paths = with pkgs; [blender simplescreenrecorder];
  };
}
