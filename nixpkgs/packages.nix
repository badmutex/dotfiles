{ pkgs
, ...
}:

let

  # to update, run:
  # nix-prefetch-git git://github.com/NixOS/nixpkgs-channels refs/heads/nixpkgs-unstable
  src = pkgs.fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs-channels";
    rev = "497e6d2f1d149f5fbe004e15fe8c384eed993943"; # 2016/12/05
    sha256 = "1mh1pmgfi6xrfhx3c7r47iip06w66r9xzqwda79z73hmgq5zbvhx";
  };

  pinned-pkgs = import src {};

in
with pinned-pkgs;

let

  inherit (stdenv.lib) optional optionals;
  inherit (stdenv) isLinux;
  myOptions = callPackage ./packageChoices.nix { };

  args = {inherit pkgs config stdenv;} // myOptions;

  gitchangelog = callPackage ./apps/gitchangelog.nix { inherit (pkgs.python27Packages) buildPythonPackage d2to1; };
  devPython27 = callPackage ./devenv/python27.nix {};
  devHaskell  = with args; callPackage ./devenv/haskell.nix {};

  emacsSetup = emacsWithPackages (self: with self; []);

in

let

  all = with args;

    ### cloudfs / synchronization
       optional withDropbox dropbox
    ++ optional withMega    megatools
    ++ optional isLinux      unison

    ### development
    ++ [ emacsSetup autoconf gnumake gcc silver-searcher colordiff direnv ]

    ### developtment / cloud
    ++ [ nixops ]

    ### development / git tools
    ++ (with gitAndTools;
       [ git hub git-extras topGit gitchangelog
         git-crypt
       ])

    # ### development / python
    ++ [ devPython27 ]
    ++ [ devHaskell  ]
    ++ [ sqlite sqlitebrowser ]

    # ### finance
    ++ (with haskellPackages;
       [ hledger
         # hledger-ui
         # hledger-web
         # hledger-diff # broken in 2016/12/05
       ])
    ++ [ gnuplot ]

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
    ++ [ mendeley ]
    ++ optional  withEvince evince
    ++ optional  withInkscape    inkscape
    ++ optionals withLatex       [texlive.combined.scheme-full biber ghostscript]
    ++ optional  withLibreOffice libreoffice
    ++ optional  withPopfile     popfile

    ### security
    ++ [ gnupg gnutls pinentry keychain keepass keepassx2 ]
    ++ optional  isLinux paperkey
    ++ optionals withYubikey [ yubikey-personalization-gui
                               yubikey-personalization ]

    ### tools
    ++ [ aspell aspellDicts.en
         bc
         graphviz-nox
         imagemagick
         inotify-tools
         rsync
         most
         tree
         unison
         unzipNLS
         zip
         # zoom-us # broken in 2016/12/05
       ]
    ++ optional isLinux aria

    ### web / network
    ++ [ httpie jq nmap w3m ]
    ++ optional withChrome   google-chrome
    ++ optional withChromium chromium

    ### X11, window management
    ++ [ screen tmux ]                     # terminal multiplexers
    ++ [ enlightenment.terminology ]       # terminals
    ++ [ xclip xsel xdotool                # X management
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
