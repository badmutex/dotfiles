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
  inherit (stdenv.lib) optional optionals;
  yubikey = [ yubikey-personalization-gui yubikey-personalization ];
in
[
  aria
  aspell
  aspellDicts.en
  autoconf
  bc
  gnumake
  gnupg
  gnutls
  iotop
  paperkey
  pinentry
  keychain
  httpie
  irssi
  jq
  nix-repl
  nmap
  screen
  texinfo
  #texLive  # core
  tmux
  terminator
  unzipNLS
  xclip
  zip
]
++ optional  withEvince      evince
++ optional  withInkscape    inkscape
++ optionals withKeepass     [keepass keepassx2 xdotool]
++ optionals withLatex       [texLiveFull biber texmaker]
++ optional  withLibreOffice libreoffice
++ optional  withSynergy     synergy
++ optional  withThunderbird thunderbird
++ optionals withYubikey     yubikey
