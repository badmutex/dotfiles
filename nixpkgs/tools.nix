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
  yubikey = callPackage ./apps/yubikey-personalization-gui.nix { qt=qt4; };
in
[
  aspell
  aspellDicts.en
  bc
  gnupg
  keychain
  irssi
  nix-repl
  patchelf
  terminator
  unzipNLS
  xclip
  zip
]
++ optional  withEvince      evince
++ optional  withInkscape    inkscape
++ optionals withKeepass     [keepass xdotool]
++ optionals withLatex       [texLiveFull biber texmaker]
++ optional  withLibreOffice libreoffice
++ optional  withSynergy     synergy
++ optional  withThunderbird thunderbird
++ optional  withYubikey     yubikey
