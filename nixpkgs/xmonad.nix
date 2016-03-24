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
  pa_applet
  parcellite
  xorg.xev
  xorg.xbacklight
  gnome3.gnome_keyring
]
