{ pkgs, ... }:

with pkgs;

[
  dmenu
  trayer
  xcompmgr
  kde4.kmix
  haskellPackages.xmobar
  networkmanagerapplet
]
