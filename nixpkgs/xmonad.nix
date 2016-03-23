{ pkgs, ... }:

with pkgs;

[
  dmenu
  trayer
  rofi
  compton
  kde4.kmix
  haskellPackages.xmobar
  networkmanagerapplet
  xorg.xev
  xorg.xbacklight
]
