{ pkgs, ... }:

with pkgs;

[
  dmenu
  trayer
  rofi
  xcompmgr
  kde4.kmix
  haskellPackages.xmobar
  networkmanagerapplet
]
