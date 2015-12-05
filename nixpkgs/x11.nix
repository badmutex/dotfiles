{ pkgs, stdenv
, ...}:

let
  inherit (stdenv.lib) optionals;
in
with pkgs;

optionals (! stdenv.isLinux) [
  spotify
  feh
  gtk
  gtk-engine-murrine
  gtk_engines
  oxygen_gtk
  lxappearance
  gnome.gnomeicontheme
  hicolor_icon_theme
  xfce.terminal
]
