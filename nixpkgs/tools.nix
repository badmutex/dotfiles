{ pkgs, stdenv
, withEvince      ? stdenv.isLinux
, withInkscape    ? stdenv.isLinux
, withLatex       ? stdenv.isLinux
, withLibreOffice ? stdenv.isLinux
, withKeepass     ? stdenv.isLinux
, withSynergy     ? stdenv.isLinux
, withThunderbird ? stdenv.isLinux
, withYubikey     ? stdenv.isLinux
, ...
}:

with pkgs;
let
  inherit (stdenv) isLinux;
  inherit (stdenv.lib) optional optionals;
  yubikey = [ yubikey-personalization-gui yubikey-personalization ];
in
[
  aspell
  aspellDicts.en
  autoconf
  bc
  graphviz-nox
  gnumake
  gnupg
  gnutls
  imagemagick
  pinentry
  keychain
  httpie
  irssi
  jq
  nix-repl
  nmap
  screen
  tmux
  terminator
  tree
  unison
  unzipNLS
  w3m
  xclip
  zip
]
++ optional  isLinux         aria
++ optional  withEvince      evince
++ optional  withInkscape    inkscape
++ optionals withKeepass     [keepassx2 xdotool] # keepass 
++ optional  isLinux         iotop
++ optionals withLatex       [texLiveFull biber texmaker]
++ optional  withLibreOffice libreoffice
++ optional  isLinux         paperkey
++ optional  withSynergy     synergy
++ optional  isLinux         terminator
++ optional  withThunderbird thunderbird
++ optional  isLinux         unison
++ optionals withYubikey     yubikey
